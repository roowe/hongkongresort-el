-module(hongkongresort_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-export([update_router/0, generate_controller/0]).
-include("common.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    app_misc:init(),
    Dispatch = dispatch(),
    {ok, _} = cowboy:start_http(http, 10, [{port, 8888}], 
                                [
                                 {env, [{dispatch, Dispatch}]},
                                 {onresponse, fun error_hook/4},
                                 {compress, true},
                                 {onrequest, fun(Req) ->
                                                     Req
                                             end},
                                 {middlewares, [cowboy_router, cowboy_handler]}
                                ]),
    mysql_misc:init(),
    load_all(),
    start_ets_cache_table(),
    {ok, SupPid} = hongkongresort_sup:start_link(),

    ok = hongkongresort_sup:start_child(mod_activity_noti),

    register(hongkongresort, self()),
    {ok, SupPid}.

stop(_State) ->
    ok.

dispatch() ->
    cowboy_router:compile([
                           {'_', generate_controller()}
                          ]).

update_router() ->
    cowboy:set_env(http, dispatch, dispatch()).

generate_controller() ->
    ControllerFiles = filelib:wildcard(filename:join([app_misc:root_dir(), "ebin/*_controller.beam"])),
    [controller_path(FileName) || FileName <- ControllerFiles].

controller_path(FileName) ->
    BaseName = filename:basename(FileName, ".beam"),
    Res = lists:sublist(BaseName, length(BaseName) - length("_controller")),
    {"/el/" ++ Res ++ "/[...]", list_to_atom(BaseName), []}.


error_hook(404, Headers, <<>>, Req) ->
    ?DEBUG("Req ~p~n", [Req]),
    Path = cowboy_req:path(Req),
    Body = ["404 Not Found: \"", Path,
            "\" is not the path you are looking for.\n"],
    Headers2 = lists:keyreplace(<<"content-length">>, 1, Headers,
                                {<<"content-length">>, integer_to_list(iolist_size(Body))}),
    cowboy_req:reply(404, Headers2, Body, Req);
error_hook(Code, Headers, <<>>, Req) when is_integer(Code), Code >= 400 ->    
    Body = ["HTTP Error ", integer_to_list(Code), $\n],
    ?DEBUG("Body ~p~n", [Body]),
    Headers2 = lists:keyreplace(<<"content-length">>, 1, Headers,
                                {<<"content-length">>, integer_to_list(iolist_size(Body))}),
    cowboy_req:reply(Code, Headers2, Body, Req);
error_hook(_Code, _Headers, _Body, Req) ->
    Req.

load_all() ->
    {ok, Modules} = application:get_key(?APP_NAME, modules),
    [code:load_file(Module) || Module <- Modules].

start_ets_cache_table() ->
    {ok, _} = ets_cache:new_cache_table(user_cache, 300),
    {ok, _} = ets_cache:new_cache_table(login_cache, 300),
    {ok, _} = ets_cache:new_cache_table(image_cache, 300).

