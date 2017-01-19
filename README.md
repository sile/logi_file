logi_file
===============

ファイル出力用の[logi](https://github.com/sile/logi)バックエンド

使い方
------

```erlang
%% infoレベル以上のログを出力する (一時間毎にローテート)
> logi_file:start_backend(info_log_file, "/path/to/log_file", [{rotate, hourly}, {suffix, yyyymmdd_hh}]).
> logi_file:install(info, info_log_file).

%% デフォルト(logi:default_logger())とは別にアクセスログをnoticeレベルを閾値値に出力する
> logi:start_logger(access_log).
> logi_file:start_backend(access_log_file, "/path/to/log_file", [{rotate, daily}, {suffix, date}]).
> logi_file:install(access_log, notice, access_log_file).

%% 登録を解除する
> logi_file:uninstall(info_log_file).
> logi_file:uninstall(access_log, access_log_file).
> logi_file:stop_backend(info_log_file).
> logi_file:stop_backend(access_log_file).
```
