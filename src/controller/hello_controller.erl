-module(hello_controller).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-export([execute_get/2, execute_post/2]).

-include("common.hrl").


init(_Type, Req, _Env) ->
	{ok, Req, undefined}.

handle(Req, State) ->
    Req1 = controller_helper:execute(?MODULE, Req),
	{ok, Req1, State}.

execute_get(KeyValues, Req) ->
    reply_misc:ok_reply(json, 
                        {[{ret, 0},
                          {msg, <<"测试成功"/utf8>>},
                          {data, {KeyValues}}]}, 
                        Req).
    

execute_post(_KeyValues, Req) ->
    reply_misc:method_not_allowed(Req).
    
terminate(_Reason, _Req, _State) ->
	ok.

