-module(ws_controller).
-behaviour(cowboy_websocket_handler).

-export([init/2]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).
-export([publish/2]).

-include("common.hrl").

init(Req, Opts) ->
    ?DEBUG("~p~n", [ds_misc:rec_to_pl(controller_helper:http_req(), Req)]),
    KeyValues = cowboy_req:parse_qs(Req),
    {_, Token} = lists:keyfind(<<"token">>, 1, KeyValues),
    %% 不知道怎么异常返回，故直接crash
    {ok, UserId} = lib_user:user_id_by_token(Token),
    subscribe(UserId),
    {cowboy_websocket, Req, undefined_state}.


%% websocket_handle({text, Msg}, Req, State) ->
%% 	{reply, {text, << "That's what she said! ", Msg/binary >>}, Req, State};
websocket_handle(Data, Req, State) ->
    ?DEBUG("_Data ~p~n", [Data]),
	{reply, Data, Req, State}.

websocket_info(test, Req, State) ->
    {reply, {text, <<"hello test">>}, Req, State};
websocket_info(_Info, Req, State) ->
    ?WARNING_MSG("Unknow Info ~p~n", [_Info]),
	{ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    ?DEBUG("Reason ~p~n", [_Reason]),
	ok.

subscribe(UserId) ->
    gproc:reg({p, l, {?MODULE, UserId}}).

publish(UserId, Msg) ->
    gproc:send({p, l, {?MODULE, UserId}}, Msg).
