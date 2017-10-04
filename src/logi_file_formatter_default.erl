%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
-module(logi_file_formatter_default).

-behaviour(logi_file_formatter).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
-export([make/0]).
-export_type([formatter/0]).

%%------------------------------------------------------------------------------------------------------------------------
%% 'logi_file_formatter' Callback API
%%------------------------------------------------------------------------------------------------------------------------
-export([format/5]).

%%------------------------------------------------------------------------------------------------------------------------
%% Macros & Records & Types
%%------------------------------------------------------------------------------------------------------------------------
-define(MAX_LOG_SIZE, 1024 * 100). % TODO: オプションで変更可能にする

-define(STATE, ?MODULE).

-record(?STATE,
        {
        }).

-opaque formatter() :: #?STATE{}.

%%------------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------------------------------------------------
-spec make() -> formatter().
make() ->
    #?STATE{}.

%%------------------------------------------------------------------------------------------------------------------------
%% 'logi_file_formatter' Callback Functions
%%------------------------------------------------------------------------------------------------------------------------
%% @private
-spec format(formatter(), logi_location:location(), logi_msg_info:info(), io:format(), [term()]) -> iodata().
format(_Formatter, Location, MsgInfo, Format, Args) ->
    Msg = io_lib:format("~s [~s] ~p ~p ~s:~s:~p [~s] ~s" ++ format_omitted(logi_msg_info:get_omitted_count(MsgInfo)) ++ "\n",
                        [format_timestamp(logi_msg_info:get_timestamp(MsgInfo)),
                         logi_msg_info:get_severity(MsgInfo),
                         logi_location:get_node(Location),
                         logi_location:get_process(Location),
                         logi_location:get_module(Location),
                         logi_location:get_function(Location),
                         logi_location:get_line(Location),
                         format_headers(logi_msg_info:get_headers(MsgInfo)),
                         re:replace(io_lib:format(Format, Args), "\\s{2,}", " ", [global])]),
    abbrev(list_to_binary(Msg), ?MAX_LOG_SIZE, <<"...\n">>).

%%------------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------------------------------------------------
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
