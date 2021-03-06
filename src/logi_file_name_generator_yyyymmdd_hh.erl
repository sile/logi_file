%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Log Filename (with "yyyymmdd_hh" suffix) Generator
%% @private
-module(logi_file_name_generator_yyyymmdd_hh).

-behaviour(logi_file_name_generator).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
-export([make/1]).

-export_type([state/0]).

%%------------------------------------------------------------------------------------------------------------------------
%% 'logi_file_name_generator' Callback API
%%------------------------------------------------------------------------------------------------------------------------
-export([generate/1]).

%%------------------------------------------------------------------------------------------------------------------------
%% Macros & Records & Types
%%------------------------------------------------------------------------------------------------------------------------
-define(STATE, ?MODULE).

-record(?STATE,
        {
          parent :: logi_file_name_generator:state()
        }).

-type state() :: #?STATE{}.

%%------------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------------------------------------------------
%% @doc インスタンスを生成する
-spec make(logi_file_name_generator:state()) -> state().
make(Parent) ->
    #?STATE{parent = Parent}.

%%------------------------------------------------------------------------------------------------------------------------
%% 'logi_file_name_generator' Callback Functions
%%------------------------------------------------------------------------------------------------------------------------
%% @private
generate(State) ->
    #?STATE{parent = Parent0} = State,
    {BasePath, Parent1} = logi_file_name_generator:generate(Parent0),
    Now = now_yyyymmdd_hh(),
    FilePath = <<BasePath/binary, ".", Now/binary>>,
    {FilePath, State#?STATE{parent = Parent1}}.

%%------------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------------------------------------------------
-spec now_yyyymmdd_hh() -> binary().
now_yyyymmdd_hh() ->
    {{Year, Month, Day}, {Hour, _, _}} = calendar:local_time(),
    list_to_binary(io_lib:format("~4..0B~2..0B~2..0B_~2..0B", [Year, Month, Day, Hour])).
