%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Log Filename (with date suffix) Generator
%% @private
-module(logi_file_name_generator_date).

-behaviour(logi_file_name_generator).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
-export([make/3]).

-export_type([state/0, place/0]).

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
          parent    :: logi_file_name_generator:state(),
          place     :: place(),
          delimiter :: binary()
        }).

-type state() :: #?STATE{}.

-type place() :: suffix | prefix.

%%------------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------------------------------------------------
%% @doc インスタンスを生成する
-spec make(logi_file_name_generator:state(), place(), string()) -> state().
make(Parent, Place, Delimiter) ->
    #?STATE{parent = Parent, place = Place, delimiter = list_to_binary(Delimiter)}.

%%------------------------------------------------------------------------------------------------------------------------
%% 'logi_file_name_generator' Callback Functions
%%------------------------------------------------------------------------------------------------------------------------
%% @private
generate(State) ->
    #?STATE{parent = Parent0, place = Place, delimiter = Delimiter} = State,
    Now = format_now_date(),
    {BasePath, Parent1} = logi_file_name_generator:generate(Parent0),
    FilePath =
        case Place of
            suffix ->
                <<BasePath/binary, Delimiter/binary, Now/binary>>;
            prefix ->
                Dir = filename:dirname(BasePath),
                Name = filename:basename(BasePath),
                <<Dir/binary, "/", Now/binary, Delimiter/binary, Name/binary>>
        end,
    {FilePath, State#?STATE{parent = Parent1}}.

%%------------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------------------------------------------------
-spec format_now_date() -> binary().
format_now_date() ->
    {{Year, Month, Day}, _} = calendar:local_time(),
    list_to_binary(io_lib:format("~4..0B~2..0B~2..0B", [Year, Month, Day])).
