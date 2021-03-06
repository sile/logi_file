%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc File Backend Interface
-module(logi_file).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
-export([
         start_backend/3,
         stop_backend/1,
         install/2, install/3,
         install_opt/3, install_opt/4,
         uninstall/1, uninstall/2
        ]).

-export_type([
              backend_id/0,
              backend_option/0, backend_options/0,
              install_opt/0, install_opts/0
             ]).

%%------------------------------------------------------------------------------------------------------------------------
%% Types
%%------------------------------------------------------------------------------------------------------------------------
-type backend_id() :: atom().

-type backend_options() :: [backend_option()].

-type backend_option() :: {rotate, hourly | daily | {daily, calendar:time()}}
                        | {suffix, yyyymmdd_hh | date | {date, Delimiter::string()}}  % default: Delimiter="."
                        | {prefix, date | {date, Delimiter::string()}}. % default: Delimiter="."

-type install_opts() :: [install_opt()].
-type install_opt()  :: {formatter, logi_file_formatter:formatter()}. % default: `logi_file_formatter_default:make()'

%%------------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------------------------------------------------
%% @doc ログ出力用のバックエンドを起動する
-spec start_backend(backend_id(), file:name_all(), backend_options()) -> ok | {error, Reason} when
      Reason :: {already_started, pid()}.
start_backend(BackendId, LogFile, Options) ->
    logi_file_backend:start(BackendId, LogFile, Options).

%% @doc バックエンドを停止する
%%
%% 対象バックエンドが未起動の場合は、エラーとならずに単に無視される
-spec stop_backend(backend_id()) -> ok.
stop_backend(BackendId) ->
    logi_file_backend:stop(BackendId).

%% @equiv install(logi:default_logger(), ConditionSpec, BackendId)
-spec install(logi_condition:spec(), backend_id()) -> ok.
install(ConditionSpec, BackendId) ->
    install(logi:default_logger(), ConditionSpec, BackendId).

%% @equiv install_opt(Logger, ConditionSpec, BackendId, [])
-spec install(logi:logger(), logi_condition:spec(), backend_id()) -> ok.
install(Logger, ConditionSpec, BackendId) ->
    install_opt(Logger, ConditionSpec, BackendId, []).

%% @equiv install_opt(logi:default_logger(), ConditionSpec, BackendId, [])
-spec install_opt(logi_condition:spec(), backend_id(), install_opts()) -> ok.
install_opt(ConditionSpec, BackendId, Options) ->
    install_opt(logi:default_logger(), ConditionSpec, BackendId, Options).

%% @doc ファイル出力用のログバックエンドをLoggerに登録する
%%
%% 既に登録済みの場合は、内容が更新される.
%%
%% なおバックエンドを登録しても、そのバックエンドが{@link start_backend/3}を使って起動されていない場合は、
%% ログが出力されないので注意が必要。
-spec install_opt(logi:logger(), logi_condition:spec(), backend_id(), install_opts()) -> ok.
install_opt(Logger, ConditionSpec, BackendId, Options) when is_atom(BackendId) ->
    Formatter = proplists:get_value(formatter, Options, logi_file_formatter_default:make()),
    logi:set_backend(Logger, {BackendId, logi_file_backend, Formatter}, ConditionSpec);
install_opt(Logger, ConditionSpec, BackendId, Options) ->
    error(badarg, [Logger, ConditionSpec, BackendId, Options]).

%% @equiv uninstall(logi:default_logger(), BackendId)
-spec uninstall(backend_id()) -> ok.
uninstall(BackendId) ->
    uninstall(logi:default_logger(), BackendId).

%% @doc バックエンドの登録を解除する
%%
%% バックエンドが未登録の場合は、エラーとはならずに単に無視される.
%%
%% また、登録を解除してもバックエンドプロセスは自動では停止しないので、
%% 必要であれば明示的に{@link stop_backend/1}を呼び出す必要がある。
-spec uninstall(logi:logger(), backend_id()) -> ok.
uninstall(Logger, BackendId) ->
    logi:delete_backend(Logger, BackendId).
