-module(hongkongresort).

-export([start/0, stop_and_halt/0]).
-include("common.hrl").

start() ->
    lager_misc:start(),
    app_misc:start(?APP_NAME).


stop_and_halt() ->
    io_misc:local_info_msg("Halting Erlang VM~n", []),
    init:stop(),
    ok.

