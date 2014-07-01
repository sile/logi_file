%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Daily Rotate Policy
%% @private
%%
%% XXX: 手抜き実装
-module(logi_file_rotator_daily).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
-export([make_mfargs/0, start_link/1]).

%%------------------------------------------------------------------------------------------------------------------------
%% Internal API
%%------------------------------------------------------------------------------------------------------------------------
-export([loop/1]).

%%------------------------------------------------------------------------------------------------------------------------
%% Records
%%------------------------------------------------------------------------------------------------------------------------
-record(state,
        {
          parent_pid :: pid()
        }).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------------------------------------------------
%% @doc プロセス起動情報を返す
-spec make_mfargs() -> logi_file_rotator:instance_mfargs().
make_mfargs() -> {?MODULE, start_link, []}.

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
            ok = schedule_next_rotate(),
            ?MODULE:loop(State)
    end.

-spec schedule_next_rotate() -> ok.
schedule_next_rotate() ->
    End = (calendar:time_to_seconds({23, 59, 59}) + 1),
    Current = calendar:time_to_seconds(element(2, calendar:local_time())),
    Delta = (End - Current) * 1000,
    _ = erlang:send_after(Delta + 100, self(), rotate_notify),
    ok.
