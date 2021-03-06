%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
-module(logi_file_formatter_plain).

-behaviour(logi_file_formatter).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
-export([make/0]).
-export_type([formatter/0]).

%%------------------------------------------------------------------------------------------------------------------------
%% 'logi_file_formatter' Callback API
%%------------------------------------------------------------------------------------------------------------------------
-export([format/5]).

%%------------------------------------------------------------------------------------------------------------------------
%% Macros & Records & Types
%%------------------------------------------------------------------------------------------------------------------------
-define(STATE, ?MODULE).

-record(?STATE,
        {
        }).

-opaque formatter() :: #?STATE{}.

%%------------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------------------------------------------------
-spec make() -> formatter().
make() ->
    #?STATE{}.

%%------------------------------------------------------------------------------------------------------------------------
%% 'logi_file_formatter' Callback Functions
%%------------------------------------------------------------------------------------------------------------------------
%% @private
-spec format(formatter(), logi_location:location(), logi_msg_info:info(), io:format(), [term()]) -> iodata().
format(_Formatter, _Location, _MsgInfo, Format, Args) ->
    [io_lib:format(Format, Args), $\n].
