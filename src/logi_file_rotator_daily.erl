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
-export([make_mfargs/0, make_mfargs/1, start_link/2]).

%%------------------------------------------------------------------------------------------------------------------------
%% Internal API
%%------------------------------------------------------------------------------------------------------------------------
-export([loop/1]).

%%------------------------------------------------------------------------------------------------------------------------
%% Macros & Records
%%------------------------------------------------------------------------------------------------------------------------
-define(ONE_DAY_SECONDS, 60 * 60 * 24).

-record(state,
        {
          parent_pid :: pid(),
          time       :: calendar:time()
        }).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------------------------------------------------
%% @equiv make_mfargs({0,0,0})
-spec make_mfargs() -> logi_file_rotator:instance_mfargs().
make_mfargs() ->
    make_mfargs({0,0,0}).


%% @doc プロセス起動情報を返す
-spec make_mfargs(calendar:time()) -> logi_file_rotator:instance_mfargs().
make_mfargs({_, _, _} = Time) ->
    {?MODULE, start_link, [Time]}.

%% @doc プロセスを起動する
-spec start_link(pid(), calendar:time()) -> {ok, pid()}.
start_link(ParentPid, Time) ->
    State = #state{parent_pid = ParentPid, time = Time},
    Pid = spawn_link(?MODULE, loop, [State]),
    {ok, Pid}.

%%------------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------------------------------------------------
-spec loop(#state{}) -> no_return().
loop(State) ->
    ok = schedule_next_rotate(State),
    receive
        rotate_notify ->
            ok = logi_file_backend:rotate(State#state.parent_pid),
            ?MODULE:loop(State)
    end.

-spec schedule_next_rotate(#state{}) -> ok.
schedule_next_rotate(#state{time = Time}) ->
    End = calendar:time_to_seconds(Time),
    Current = calendar:time_to_seconds(element(2, calendar:local_time())),
    Delta = case Current < End of
                true  -> End - Current;
                false -> (?ONE_DAY_SECONDS + End) - Current
            end,
    _ = erlang:send_after(Delta * 1000 + 100, self(), rotate_notify),
    ok.
