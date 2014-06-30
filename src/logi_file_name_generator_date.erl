%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Log Filename (with date suffix) Generator
%% @private
-module(logi_file_name_generator_date).

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
          filename :: binary()
        }).

-type state() :: #?STATE{}.

%%------------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------------------------------------------------
%% @doc インスタンスを生成する
-spec make(file:name_all()) -> state().
make(FileName) ->
    #?STATE{filename = iolist_to_binary(FileName)}.

%%------------------------------------------------------------------------------------------------------------------------
%% 'logi_file_name_generator' Callback Functions
%%------------------------------------------------------------------------------------------------------------------------
%% @private
generate(State) ->
    Name = <<(State#?STATE.filename)/binary, ".", (format_now_date())/binary>>,
    {Name, State}.

%%------------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------------------------------------------------
-spec format_now_date() -> binary().
format_now_date() -> 
    {{Year, Month, Day}, _} = calendar:local_time(),
    list_to_binary(io_lib:format("~4..0B~2..0B~2..0B", [Year, Month, Day])).
