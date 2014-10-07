-module(echo_controller).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-include("common.hrl").

init({tcp, http}, _Req, _Opts) ->
	{upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    %% register(?MODULE, self()),
    %% gproc:reg({p, l, {?MODULE, UserId}}),
    ?DEBUG("_TransportName, Req, _Opts ~p~n", [{_TransportName, Req, _Opts}]),
	%% erlang:start_timer(1000, self(), <<"Hello!">>),
	{ok, Req, undefined_state}.

%% websocket_handle({text, Msg}, Req, State) ->
%% 	{reply, {text, << "That's what she said! ", Msg/binary >>}, Req, State};
websocket_handle(Data, Req, State) ->
    ?DEBUG("_Data ~p~n", [Data]),
	{reply, Data, Req, State}.

websocket_info(timeout, Req, State) ->
	%% erlang:start_timer(1000, self(), <<"How' you doin'?">>),
	{reply, {text, <<"Hello World">>}, Req, State};
websocket_info(_Info, Req, State) ->
    ?DEBUG("Info ~p~n", [_Info]),
	{reply, [{text, <<"test">>}, {text, "test2"}], Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    ?DEBUG("Reason ~p~n", [_Reason]),
	ok.

%% test() ->    
%%     gproc:send({p, l, {?MODULE, UserId}}, {?MODULE, test, test}).
