%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc File backend for logi
%% @private
-module(logi_file_backend).

-behaviour(logi_backend).
-behaviour(gen_server).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
-export([
         start/3,
         stop/1
        ]).

%%------------------------------------------------------------------------------------------------------------------------
%% Application Internal API
%%------------------------------------------------------------------------------------------------------------------------
-export([start_link_impl/3, rotate/1]).

%%------------------------------------------------------------------------------------------------------------------------
%% 'logi_backend' Callback API
%%------------------------------------------------------------------------------------------------------------------------
-export([write/5]).

%%------------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback API
%%------------------------------------------------------------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%------------------------------------------------------------------------------------------------------------------------
%% Macros & Records
%%------------------------------------------------------------------------------------------------------------------------
-define(LOGFILE_EXIISTENCE_CHECK_INTERVAL, 10 * 1000).
-define(MAX_LOG_SIZE, 1024). % TODO: オプションで変更可能にする

-record(state,
        {
          logfilename        :: binary(),
          logfile_io         :: file:io_device(),
          rotator_pid        :: pid(),
          filename_generator :: logi_file_name_generator:state()
        }).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------------------------------------------------
%% @doc ファイル出力用のバックエンドプロセスを起動する
-spec start(logi_file:backend_id(), file:name_all(), logi_file:backend_options()) -> ok | {error, Reason} when
      Reason :: {already_started, pid()}.
start(BackendId, LogFile, Options) when is_atom(BackendId), is_list(Options) ->
    Rotator       = select_logfile_rotator(Options),
    NameGenerator = build_logfilename_generator(logi_file_name_generator_plain:make(LogFile), Options),
    case logi_file_backend_sup:start_child(BackendId, Rotator, NameGenerator) of
        {error, {already_started, Pid}} -> {error, {already_started, Pid}};
        {ok, _Pid}                      -> ok;
        {error, Reason}                 -> exit(Reason)  % 予想外のエラー
    end;
start(BackendId, LogFile, Options) ->
    error(badarg, [BackendId, LogFile, Options]). % まだ引数チェックは簡易的

%% @doc バックエンドプロセスを停止する
-spec stop(logi_file:backend_id()) -> ok.
stop(BackendId) ->
    logi_file_backend_sup:stop_child(BackendId).

%%------------------------------------------------------------------------------------------------------------------------
%% Application Internal Functions
%%------------------------------------------------------------------------------------------------------------------------
%% @doc logi_file_backend_supから呼び出される実際のプロセス起動関数
-spec start_link_impl(logi_file:backend_id(), logi_file_rotator:instance_mfargs(), logi_file_name_generator:state()) ->
                        {ok, pid()} | {error, Reason} when
      Reason :: {already_started, pid()} | term().
start_link_impl(BackendId, Rotator, NameGenerator) ->
    gen_server:start_link({local, BackendId}, ?MODULE, [Rotator, NameGenerator], []).

%% @doc ログファイルローテーターがファイルをローテートするタイミングを通知するために使用する関数
-spec rotate(pid()) -> ok.
rotate(BackendPid) ->
    gen_server:cast(BackendPid, rotate).

%%------------------------------------------------------------------------------------------------------------------------
%% 'logi_backend' Callback Functions
%%------------------------------------------------------------------------------------------------------------------------
%% @private
-spec write(logi_backend:backend(), logi_location:location(), logi_msg_info:info(), io:format(), [term()]) -> any().
write(Backend, Location, MsgInfo, Format, Args) ->
    %% TODO: formatterを指定可能にする
    Msg =
        io_lib:format("~s [~s] ~p ~p ~s:~p [~s] ~s" ++ format_omitted(logi_msg_info:get_omitted_count(MsgInfo)) ++ "\n",
                      [format_timestamp(logi_msg_info:get_timestamp(MsgInfo)),
                       logi_msg_info:get_severity(MsgInfo),
                       logi_location:get_node(Location),
                       logi_location:get_process(Location),
                       logi_location:get_module(Location),
                       logi_location:get_line(Location),
                       format_headers(logi_msg_info:get_headers(MsgInfo)),
                       [re:replace(io_lib:format(Format, Args), "\\s+", " ", [global])]]),
    MsgBin = abbrev(list_to_binary(Msg), ?MAX_LOG_SIZE, <<"...">>),
    gen_server:cast(logi_backend:get_process(Backend), {write, MsgBin}).

%%------------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback Functions
%%------------------------------------------------------------------------------------------------------------------------
%% @private
init([Rotator, NameGenerator0]) ->
    %% TODO: compressedをオプション化
    %% TODO: delayed_writeをオプション化
    %% TODO: 出力レート制御(上限)を指定可能にする
    case logi_file_rotator:start_link(Rotator, self()) of
        {error, Reason}  -> {stop, {start_logfile_rotator_failed, Rotator, Reason}};
        {ok, RotatorPid} ->
            {FileName, NameGenerator1} = logi_file_name_generator:generate(NameGenerator0),
            case open_logfile(FileName) of
                {error, Reason} -> {stop, {cannot_open_file, FileName, Reason}};
                {ok, IoDevice}  ->
                    State =
                        #state{
                           logfilename        = FileName,
                           logfile_io         = IoDevice,
                           rotator_pid        = RotatorPid,
                           filename_generator = NameGenerator1
                          },
                    ok = schedule_file_existence_check(),
                    {ok, State}
            end
    end.

%% @private
handle_call(_Request, _From, State) ->
    {noreply, State}.

%% @private
handle_cast({write, Msg}, State) ->
    ok = do_write(Msg, State),
    {noreply, State};
handle_cast(rotate, State0) ->
    State1 = do_rotate(State0),
    {noreply, State1};
handle_cast(_Request, State) ->
    {noreply, State}.

%% @private
handle_info(logfile_existence_check, State0) ->
    State1 = do_logfile_existence_check(State0),
    ok = schedule_file_existence_check(),
    {noreply, State1};
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, State) ->
    ok = logi_file_rotator:stop(State#state.rotator_pid),
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%------------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------------------------------------------------
-spec schedule_file_existence_check() -> ok.
schedule_file_existence_check() ->
    _ = erlang:send_after(?LOGFILE_EXIISTENCE_CHECK_INTERVAL, self(), logfile_existence_check),
    ok.

-spec select_logfile_rotator(logi_file:backend_options()) -> logi_file_rotator:instance_mfargs().
select_logfile_rotator(Options) ->
    case proplists:get_value(rotate, Options, undefined) of
        undefined     -> logi_file_rotator_none:make_mfargs();
        daily         -> logi_file_rotator_daily:make_mfargs();
        {daily, Time} -> logi_file_rotator_daily:make_mfargs(Time);
        _             -> error(badarg, [Options])
    end.

-spec build_logfilename_generator(logi_file_name_generator:state(), logi_file:backend_options()) ->
                                         logi_file_name_generator:state().
build_logfilename_generator(Parent, []) ->
    Parent;
build_logfilename_generator(Parent, [{suffix, date} | Options]) ->
    build_logfilename_generator(Parent, [{suffix, {date, "."}} | Options]);
build_logfilename_generator(Parent, [{suffix, {date, Delim}} | Options]) ->
    This = logi_file_name_generator_date:make(Parent, suffix, Delim),
    build_logfilename_generator(This, Options);
build_logfilename_generator(Parent, [{prefix, date} | Options]) ->
    build_logfilename_generator(Parent, [{prefix, {date, "."}} | Options]);
build_logfilename_generator(Parent, [{prefix, {date, Delim}} | Options]) ->
    This = logi_file_name_generator_date:make(Parent, prefix, Delim),
    build_logfilename_generator(This, Options);
build_logfilename_generator(Parent, [_ | Options]) ->
    build_logfilename_generator(Parent, Options).

-spec do_logfile_existence_check(#state{}) -> #state{}.
do_logfile_existence_check(State0) ->
    case filelib:is_regular(State0#state.logfilename) of
        true  -> State0;
        false -> reopen_logfile(State0)
    end.

-spec do_write(binary(), #state{}) -> ok.
do_write(Msg, State) ->
    case file:write(State#state.logfile_io, Msg) of
        ok              -> ok;
        {error, Reason} -> error({file_write_failed, [{file, State#state.logfilename}, {reason, Reason}]}, [Msg, State])
    end.

-spec do_rotate(#state{}) -> #state{}.
do_rotate(State) ->
    {FileName, NameGenerator} = logi_file_name_generator:generate(State#state.filename_generator),
    _ = logi:verbose("rotate log file: new_file=~s", [FileName]),
    reopen_logfile(State#state{logfilename = FileName, filename_generator = NameGenerator}).

-spec open_logfile(binary()) -> {ok, file:io_device()} | {error, Reason::term()}.
open_logfile(FileName) ->
    case filelib:ensure_dir(FileName) of
        {error, Reason} -> {error, Reason};
        ok              -> file:open(FileName, [append, raw, delayed_write])
    end.

-spec reopen_logfile(#state{}) -> #state{}.
reopen_logfile(State) ->
    _ = file:close(State#state.logfile_io),
    case open_logfile(State#state.logfilename) of
        {error, Reason} -> error({file_reopen_failed, [{file, State#state.logfilename}, {reason, Reason}]}, [State]);
        {ok, IoDevice}  -> State#state{logfile_io = IoDevice}
    end.

%% TODO: formatter系は共通化する
-spec format_timestamp(erlang:timestamp()) -> iodata().
format_timestamp(Timestamp) ->
    {_, _, Micros} = Timestamp,
    Millis = Micros div 1000,
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:now_to_local_time(Timestamp),
    io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B.~3..0B",
                  [Year, Month, Day, Hour, Minute, Second, Millis]).

-spec format_headers(logi:headers()) -> iodata().
format_headers(Headers) ->
    string:join([[atom_to_list(K),"=",to_string(V)] || {K, V} <- Headers], ",").

-spec format_omitted(non_neg_integer()) -> string().
format_omitted(0) -> "";
format_omitted(N) -> " (" ++ integer_to_list(N) ++ " omitted)".

-spec to_string(term()) -> string().
to_string(V) when is_binary(V)   -> binary_to_list(V);
to_string(V) when is_atom(V)     -> atom_to_list(V);
to_string(V) when is_integer(V)  -> integer_to_list(V);
to_string(V) when is_float(V)    -> float_to_list(V);
to_string(V) when is_function(V) -> erlang:fun_to_list(V);
to_string(V) when is_pid(V)      -> erlang:pid_to_list(V);
to_string(V) when is_port(V)     -> erlang:port_to_list(V);
to_string(V) when is_reference(V)-> erlang:ref_to_list(V);
to_string(V) when is_list(V)     ->
    IsNonNegInteger = fun (C) -> is_integer(C) andalso C >= 0 end,
    case lists:all(IsNonNegInteger, V) of
        true  -> V;
        false -> lists:flatten(io_lib:format("~w", [V]))
    end;
to_string(V) ->
    lists:flatten(io_lib:format("~w", [V])).

-spec abbrev(Input::binary(), MaxLength::non_neg_integer(), Ellipsis::binary()) -> binary().
abbrev(<<Bin/binary>>, MaxLength, <<Ellipsis/binary>>) when is_integer(MaxLength), MaxLength >= 0 ->
    case byte_size(Bin) =< MaxLength of
        true  -> Bin;
        false ->
            EllipsisSize = byte_size(Ellipsis),
            TruncateSize = max(0, MaxLength - EllipsisSize),
            <<(binary:part(Bin, 0, TruncateSize))/binary, Ellipsis/binary>>
    end.
