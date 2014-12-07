-module(main).

-include("common.hrl"). 

-export([
         start/0,
         started/0,
         is_running/0, is_running/1,
         stop_and_halt/0
        ]).

start() ->
    application:load(?APP_NAME),
    log_misc:start_lager(app_misc:get_env(log_level, info)),
    app_misc:start(?APP_NAME).

started() -> 
    {ok, is_running()}.

is_running() -> 
    is_running(node()).

is_running(Node) -> 
    node_misc:is_process_running(Node, hongkongresort).
    
stop_and_halt() ->
    io_misc:local_info_msg("Halting Erlang VM~n", []),
    init:stop(),
    ok.

