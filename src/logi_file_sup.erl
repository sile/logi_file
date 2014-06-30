%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Supervisor Module
%% @private
-module(logi_file_sup).

-behaviour(supervisor).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
-export([start_link/0]).

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

%%------------------------------------------------------------------------------------------------------------------------
%% 'supervisor' Callback Functions
%%------------------------------------------------------------------------------------------------------------------------
%% @hidden
init([]) ->
    BackendSup = logi_file_backend_sup,
    Children =
        [{BackendSup, {BackendSup, start_link, []}, permanent, 5000, supervisor, [BackendSup]}],
    {ok, { {one_for_one, 5, 10}, Children} }.
