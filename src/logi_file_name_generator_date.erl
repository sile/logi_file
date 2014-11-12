%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Log Filename (with date suffix) Generator
%% @private
-module(logi_file_name_generator_date).

-behaviour(logi_file_name_generator).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
-export([make/4]).

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
          parent      :: logi_file_name_generator:state(),
          place       :: place(),
          delimiter   :: binary(),
          rotate_time :: calendar:time()
        }).

-type state() :: #?STATE{}.

-type place() :: suffix | prefix.

%%------------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------------------------------------------------
%% @doc インスタンスを生成する
-spec make(logi_file_name_generator:state(), place(), string(), calendar:time()) -> state().
make(Parent, Place, Delimiter, RotateTime) ->
    #?STATE{parent = Parent, place = Place, delimiter = list_to_binary(Delimiter), rotate_time = RotateTime}.

%%------------------------------------------------------------------------------------------------------------------------
%% 'logi_file_name_generator' Callback Functions
%%------------------------------------------------------------------------------------------------------------------------
%% @private
generate(State) ->
    #?STATE{parent = Parent0, place = Place, delimiter = Delimiter} = State,
    {BasePath, Parent1} = logi_file_name_generator:generate(Parent0),
    FilePath =
        case Place of
            suffix ->
                Now = format_date(adjusted_now_date(State)),
                <<BasePath/binary, Delimiter/binary, Now/binary>>;
            prefix ->
                Now = format_date_short(adjusted_now_date(State)),
                Dir = filename:dirname(BasePath),
                Name = filename:basename(BasePath),
                <<Dir/binary, "/", Now/binary, Delimiter/binary, Name/binary>>
        end,
    {FilePath, State#?STATE{parent = Parent1}}.

%%------------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------------------------------------------------
-spec format_date(calendar:date()) -> binary().
format_date({Year, Month, Day}) ->
    list_to_binary(io_lib:format("~4..0B~2..0B~2..0B", [Year, Month, Day])).

-spec format_date_short(calendar:date()) -> binary().
format_date_short({Year, Month, Day}) ->
    list_to_binary(io_lib:format("~2..0B~2..0B~2..0B", [Year rem 100, Month, Day])).

-spec adjusted_now_date(#?STATE{}) -> calendar:date().
adjusted_now_date(#?STATE{rotate_time = Time}) ->
    Offset = calendar:time_to_seconds(Time),
    NowSeconds = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
    {Date, _} = calendar:gregorian_seconds_to_datetime(NowSeconds - Offset),
    Date.
