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
    NameGenerator = build_logfilename_generator(logi_file_name_generator_plain:make(LogFile), Options, Options),
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
    Formatter = logi_backend:get_data(Backend),
    Msg = logi_file_formatter:format(Formatter, Location, MsgInfo, Format, Args),
    gen_server:cast(logi_backend:get_process(Backend), {write, Msg}).

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
        hourly        -> logi_file_rotator_hourly:make_mfargs();
        daily         -> logi_file_rotator_daily:make_mfargs();
        {daily, Time} -> logi_file_rotator_daily:make_mfargs(Time);
        _             -> error(badarg, [Options])
    end.

-spec get_logfile_rotate_time(logi_file:backend_options()) -> calendar:time().
get_logfile_rotate_time(Options) ->
    case proplists:get_value(rotate, Options, undefined) of
        {daily, Time} -> Time;
        _             -> {0, 0, 0}
    end.

-spec build_logfilename_generator(logi_file_name_generator:state(), Optoins, Optoins) ->
                                         logi_file_name_generator:state() when
      Optoins :: logi_file:backend_options().
build_logfilename_generator(Parent, _Original, []) ->
    Parent;
build_logfilename_generator(Parent, Original, [{suffix, date} | Options]) ->
    build_logfilename_generator(Parent, Original, [{suffix, {date, "."}} | Options]);
build_logfilename_generator(Parent, Original, [{suffix, {date, Delim}} | Options]) ->
    RotateTime = get_logfile_rotate_time(Original),
    This = logi_file_name_generator_date:make(Parent, suffix, Delim, RotateTime),
    build_logfilename_generator(This, Original, Options);
build_logfilename_generator(Parent, Original, [{suffix, yyyymmdd_hh} | Options]) ->
    This = logi_file_name_generator_yyyymmdd_hh:make(Parent),
    build_logfilename_generator(This, Original, Options);
build_logfilename_generator(Parent, Original, [{prefix, date} | Options]) ->
    build_logfilename_generator(Parent, Original, [{prefix, {date, "."}} | Options]);
build_logfilename_generator(Parent, Original, [{prefix, {date, Delim}} | Options]) ->
    RotateTime = get_logfile_rotate_time(Original),
    This = logi_file_name_generator_date:make(Parent, prefix, Delim, RotateTime),
    build_logfilename_generator(This, Original, Options);
build_logfilename_generator(Parent, Original, [_ | Options]) ->
    build_logfilename_generator(Parent, Original, Options).

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
