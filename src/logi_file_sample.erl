%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Sample Module
-module(logi_file_sample).

-compile({parse_transform, logi_transform}).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
-export([
         start/1,
         stop/0,
         log/2
        ]).

%%------------------------------------------------------------------------------------------------------------------------
%% Macros
%%------------------------------------------------------------------------------------------------------------------------
-define(SAMPLE_BACKEND, logi_file_sample_backend).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------------------------------------------------
-spec start(file:name_all()) -> ok | {error, Reason::term()}.
start(LogFile) ->
    case logi_file:start_backend(?SAMPLE_BACKEND, LogFile, [{rotate, daily}, {suffix, date}]) of
        {error, Reason} -> {error, Reason};
        ok              -> logi_file:install(debug, ?SAMPLE_BACKEND)
    end.

-spec stop() -> ok.
stop() ->
    ok = logi_file:uninstall(?SAMPLE_BACKEND),
    logi_file:stop_backend(?SAMPLE_BACKEND).

-spec log(io:format(), [term()]) -> ok.
log(Format, Args) ->
    _ = logi:info(Format, Args),
    ok.

