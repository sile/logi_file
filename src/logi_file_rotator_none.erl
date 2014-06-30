%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc No Rotate Policy
%% @private
-module(logi_file_rotator_none).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
-export([make_mfargs/0, start_link/1]).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------------------------------------------------
%% @doc プロセス起動情報を返す
-spec make_mfargs() -> logi_file_rotator:instance_mfargs().
make_mfargs() -> {?MODULE, start_link, []}.

%% @doc プロセスを起動する
-spec start_link(pid()) -> {ok, pid()}.
start_link(_ParentPid) ->
    Pid = spawn_link(timer, sleep, [infinity]),
    {ok, Pid}.
