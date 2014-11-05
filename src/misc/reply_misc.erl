-module(reply_misc).

-export([render/2, render/3]).

-export([bad_request/1, bad_request/2]).

-export([method_not_allowed/1, internal_server_error/1]).

-export([ok_reply/3]).

-include("common.hrl").
-include("db_base_error_list.hrl").

render(Module, Req) ->
    render(Module, [], Req).

render(Module, Args, Req) ->
    case Module:render(Args) of
        {ok, Content} ->
            cowboy_req:reply(?HTTP_OK, [content_type(html)], Content, Req);
        {error, Err} ->
            ?WARNING_MSG("render error, Module ~p, Args ~p, Err ~p", [Module, Args, Err]),
            cowboy_req:reply(?HTTP_INTERNAL_SERVER_ERROR, Req)
    end.

bad_request(Reason, Req) ->
    cowboy_req:reply(?HTTP_BAD_REQUEST, [], Reason, Req).

bad_request(Req) ->
    bad_request(<<"bad request">>, Req).

internal_server_error(Req) ->
    cowboy_req:reply(?HTTP_INTERNAL_SERVER_ERROR, Req).

method_not_allowed(Req) ->
    cowboy_req:reply(?HTTP_METHOD_NOT_ALLOWED, Req).

ok_reply(Type, Content, Req) ->
    cowboy_req:reply(?HTTP_OK, [content_type(Type)], content(Type, Content), Req).

%% content(json, {KVs}) ->
%%     Json = 
%%         case proplists:get_value(ret, KVs) of
%%             undefined ->
%%                 {KVs};
%%             Ret ->
%%                 case data_base_error_list:get(Ret) of
%%                     [] ->
%%                         {[{desc, <<"未更新的错误码"/utf8>>} |KVs]};
%%                     #base_error_list{
%%                        error_desc = Desc
%%                       } ->
%%                         {[{desc, Desc} |KVs]}
%%                 end
%%         end,
%%     jiffy:encode(Json);
content(json, Json) ->
    jiffy:encode(Json);
content(_, Content) ->
    Content.

content_type(html) ->
    {<<"content-type">>, <<"text/html; charset=utf-8">>};
content_type(json) ->
    {<<"content-type">>, <<"application/json; charset=utf-8">>};
content_type(plain) ->
    {<<"content-type">>, <<"text/plain; charset=utf-8">>}.


%% Date
%% Content-Type
%% Server
%% and either Content-Length, Transfer-Encoding or Connection: close.

%% 1 服务端需要返回一段普通文本给客户端  普通文本 : text/plain  
%% 2 服务端需要返回一段HTML代码给客户端  HTML代码 : text/html  
%% 3 服务端需要返回一段XML代码给客户端 XML代码 : text/xml  
%% 4 服务端需要返回一段javascript代码给客户端  application/javascript
%% 5 服务端需要返回一段json串给客户端 Content-Type = 'application/json;charset=UTF-8' 
