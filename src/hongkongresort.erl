-module(hongkongresort).

-export([start/0, stop_and_halt/0]).
-include("common.hrl").

start() ->
    application:load(?APP_NAME),
    log_misc:start_lager(app_misc:get_env(log_level, info)),
    app_misc:start(?APP_NAME).


stop_and_halt() ->
    io_misc:local_info_msg("Halting Erlang VM~n", []),
    init:stop(),
    ok.

