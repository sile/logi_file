%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Supervisor for Backend Processes
%% @private
-module(logi_file_backend_sup).

-behaviour(supervisor).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
-export([start_link/0, start_child/3, stop_child/1]).

%%------------------------------------------------------------------------------------------------------------------------
%% 'supervisor' Callback API
%%------------------------------------------------------------------------------------------------------------------------
-export([init/1]).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------------------------------------------------
%% @doc Starts root supervisor
-spec start_link() -> {ok, pid()} | {error, Reason::term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc Start Child Process
-spec start_child(logi_file:backend_id(), logi_file_rotator:instance_mfargs(), logi_file_name_generator:state()) ->
                         {ok, pid()} | {error, Reason} when
      Reason :: {already_started, pid()} | term().
start_child(BackendId, Rotator, NameGenerator) ->
    supervisor:start_child(?MODULE, [BackendId, Rotator, NameGenerator]).

%% @doc Stop Child Process
-spec stop_child(logi_file:backend_id()) -> ok.
stop_child(BackendId) ->
    case whereis(BackendId) of
        undefined -> ok;
        Pid       ->
            _ = supervisor:terminate_child(?MODULE, Pid),
            ok
    end.

%%------------------------------------------------------------------------------------------------------------------------
%% 'supervisor' Callback Functions
%%------------------------------------------------------------------------------------------------------------------------
%% @hidden
init([]) ->
    Backend = logi_file_backend,
    Children =
        [{Backend, {Backend, start_impl, []}, permanent, 5000, worker, [Backend]}],
    {ok, { {simple_one_for_one, 5, 10}, Children} }.
