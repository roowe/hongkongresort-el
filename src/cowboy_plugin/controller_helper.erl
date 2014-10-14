-module(controller_helper).
-behaviour(cowboy_sub_protocol).

-export([http_req/0]).

-export([upgrade/6]).

-export([execute/2, execute/3]).

-include("common.hrl").
-include("define_info_0.hrl").

-record(http_req, {
	%% Transport.
	socket = undefined :: any(),
	transport = undefined :: undefined | module(),
	connection = keepalive :: keepalive | close,

	%% Request.
	pid = undefined :: pid(),
	method = <<"GET">> :: binary(),
	version = 'HTTP/1.1', 
	peer = undefined ,
	host = undefined ,
	host_info = undefined ,
	port = undefined ,
	path = undefined ,
	path_info = undefined ,
	qs = undefined :: binary(),
	bindings = undefined ,
	headers = [] ,
	meta = [] :: [{atom(), any()}],

	%% Request body.
	body_state = waiting ,
	buffer = <<>> :: binary(),
	multipart = undefined :: undefined | {binary(), binary()},

	%% Response.
	resp_compress = false :: boolean(),
	resp_state = waiting :: locked | waiting | waiting_stream
		| chunks | stream | done,
	resp_headers = [] ,
	resp_body = <<>> ,

	%% Functions.
	onresponse = undefined 
}).

http_req() ->
    record_info(fields, http_req).

upgrade(Req, Env, Handler, HandlerState, _Timeout, _Hibernate) ->
    {ok, execute(Handler, Req, HandlerState), Env}.

%% 并非符合通用规则，但是目前项目而言，确实是如此的规则
execute(Module, Req) ->
    execute(Module, Req, []).

execute(Module, Req, Opts) ->
    ?DEBUG("~p~n", [ds_misc:rec_to_pl(http_req(), Req)]),
    case cowboy_req:method(Req) of
        <<"GET">> ->
            %% 字段一多，ParameterList可以考虑用record
            PathInfo = cowboy_req:path_info(Req),
            AllParameterList = proplists:get_value(get_parameter, Opts, []),
            case proplists:get_value(PathInfo, AllParameterList) of
                undefined ->
                    ?WARNING_MSG("Unknow PathInfo ~p~n", [PathInfo]),    
                    reply_misc:ok_reply(json, 
                                        ?JSON([{ret, ?INFO_ACTION_MISS}]),
                                        Req);
                ParameterList ->
                    KeyValues = cowboy_req:parse_qs(Req),
                    case parse_parameter(KeyValues, ParameterList) of
                        {fail, Reason} ->
                            reply_misc:ok_reply(json, 
                                                ?JSON([{ret, Reason}]),
                                                Req);
                        {ok, ValueList} ->
                            ?INFO_MSG("GET Module ~p, PathInfo ~p, KeyValues ~p~n", [Module, PathInfo, KeyValues]),
                            case erlang:function_exported(Module, execute_get, 3) of
                                true ->
                                    case Module:execute_get(PathInfo, ValueList, Req) of
                                        {pack, ToPackData} ->
                                            reply({json, Module:get_pack(PathInfo, ToPackData)}, Req);
                                        Other ->
                                            reply(Other, Req)
                                    end;
                                false ->
                                    reply_misc:method_not_allowed(Req)
                            end
                    end
            end;
        <<"POST">> ->
            %% TODO 支持Parameter
            execute_post(Module, Req, Opts);
        OtherMethod ->
            ?WARNING_MSG("Other Method ~ts~n", [OtherMethod]),
            reply_misc:method_not_allowed(Req)
    end.


execute_post(Module, Req, Opts) ->
    PathInfo = cowboy_req:path_info(Req),
    AllParameterList = proplists:get_value(post_parameter, Opts, []),
    case proplists:get_value(PathInfo, AllParameterList) of
        undefined ->
            ?WARNING_MSG("Unknow PathInfo ~p~n", [PathInfo]),    
            reply_misc:ok_reply(json, 
                                ?JSON([{ret, ?INFO_ACTION_MISS}]),
                                Req);
        ParameterList ->
            case cowboy_req:has_body(Req) of
                true ->
                    {ok, KeyValues, Req1} = cowboy_req:body_qs(Req),
                    case parse_parameter(KeyValues, ParameterList) of
                        {fail, Reason} ->
                            reply_misc:ok_reply(json, 
                                                ?JSON([{ret, Reason}]),
                                                Req1);
                        {ok, ValueList} ->
                            ?INFO_MSG("POST Module ~p, PathInfo ~p, KeyValues ~p~n", [Module, PathInfo, KeyValues]),
                            case erlang:function_exported(Module, execute_post, 3) of
                                true ->
                                    case Module:execute_post(PathInfo, ValueList, Req1) of
                                        {pack, ToPackData} ->
                                            reply({json, Module:post_pack(PathInfo, ToPackData)}, Req1);
                                        Other ->
                                            reply(Other, Req1)
                                    end;
                                false ->
                                    reply_misc:method_not_allowed(Req1)
                            end
                    end;
                false ->
                    reply_misc:ok_reply(json, 
                                        ?JSON([{ret, ?INFO_BODY_NOT_EXIST}]),
                                        Req)
            end
    end.


parse_parameter(KeyValues, ParameterList) ->
    parse_parameter(KeyValues, ParameterList, []).

parse_parameter(_, [], Acc) ->
    {ok, lists:reverse(Acc)};
parse_parameter(KeyValues, [{Parameter, DataType, OptionalOrRequired} | ParameterList], Acc) ->
    Value = proplists:get_value(Parameter, KeyValues),
    if
        (Value =:= undefined orelse
         Value =:= <<>>) andalso 
        OptionalOrRequired =:= required ->
            {fail, ?INFO_PARAMETER_MISS};
        true ->
            parse_parameter(KeyValues, ParameterList, [decoder(DataType, Value)|Acc])            
    end.

decoder(int, Value) ->
    erlang:binary_to_integer(Value);
decoder(_, Value) ->
    Value.

reply({fail, Reason}, Req) ->
    reply_misc:ok_reply(json, 
                        ?JSON([{ret, Reason}]),
                        Req);
reply({json, JSON}, Req) ->
    reply_misc:ok_reply(json, 
                        ?JSON([{ret, ?INFO_OK}, {response, JSON}]),
                        Req);
reply(Other, _Req) ->
    Other.
