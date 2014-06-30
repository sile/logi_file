logi_file
===============

ファイル出力用の[logi](https://github.com/sile/logi)バックエンド

使い方
------

```erlang
%% infoレベル以上のログを出力する
> logi_fild:install(info, "/path/to/log_file", [{locate, daily}, {suffix, date}]).

%% デフォルト(logi:default_logger())とは別にアクセスログをnoticeレベルを閾値値に出力する
> logi:start_logger(access_log).
> logi_fild:install(access_log, notice, "/path/to/log_file", [{locate, daily}, {suffix, date}]).

%% 登録を解除する
> logi_file:uninstall().
> logi_file:uninstall(access_log).
```
