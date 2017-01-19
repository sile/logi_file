%% @copyright 2017 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Hourly Rotate Policy
%% @private
-module(logi_file_rotator_hourly).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
-export([make_mfargs/0, start_link/1]).

%%------------------------------------------------------------------------------------------------------------------------
%% Internal API
%%------------------------------------------------------------------------------------------------------------------------
-export([loop/1]).

%%------------------------------------------------------------------------------------------------------------------------
%% Macros & Records
%%------------------------------------------------------------------------------------------------------------------------
-define(ONE_HOUR_SECONDS, 60 * 60).

-record(state,
        {
          parent_pid :: pid()
        }).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------------------------------------------------
%% @doc プロセス起動情報を返す
-spec make_mfargs() -> logi_file_rotator:instance_mfargs().
make_mfargs() ->
    {?MODULE, start_link, []}.

%% @doc プロセスを起動する
-spec start_link(pid()) -> {ok, pid()}.
start_link(ParentPid) ->
    State = #state{parent_pid = ParentPid},
    Pid = spawn_link(?MODULE, loop, [State]),
    {ok, Pid}.

%%------------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------------------------------------------------
-spec loop(#state{}) -> no_return().
loop(State) ->
    ok = schedule_next_rotate(),
    receive
        rotate_notify ->
            ok = logi_file_backend:rotate(State#state.parent_pid),
            ?MODULE:loop(State)
    end.

-spec schedule_next_rotate() -> ok.
schedule_next_rotate() ->
    {_, {_, Minutes, Seconds}} = calendar:local_time(),
    Delta = ?ONE_HOUR_SECONDS - (Minutes * 60 + Seconds),
    _ = erlang:send_after(Delta * 1000 + 100, self(), rotate_notify),
    ok.
