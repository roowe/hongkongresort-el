-module(controller_helper).

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


%% 并非符合通用规则，但是目前项目而言，确实是如此的规则
execute(Module, Req) ->
    execute(Module, Req, []).

execute(Module, Req, Opts) ->
    ?DEBUG("~p~n", [ds_misc:rec_to_pl(record_info(fields, http_req), Req)]),
    case cowboy_req:method(Req) of
        <<"GET">> ->
            %% 字段一多，ParameterList可以考虑用record
            ParameterList = proplists:get_value(get_parameter, Opts, []),
            KeyValues = cowboy_req:parse_qs(Req),
            case parse_parameter(KeyValues, ParameterList) of
                {fail, Reason} ->
                    reply_misc:ok_reply(json, 
                                        {[{ret, Reason}]},
                                        Req);
                {ok, ValueList} ->
                    Action = cowboy_req:binding(action, Req),
                    ?DEBUG("GET Module ~p, Action ~p, KeyValues ~p~n", [Module, Action, KeyValues]),
                    case erlang:function_exported(Module, execute_get, 3) of
                        true ->
                            Module:execute_get(Action, ValueList, Req);
                        false ->
                            reply_misc:method_not_allowed(Req)
                    end
            end;            
        <<"POST">> ->
            %% TODO 支持Parameter
            case cowboy_req:has_body(Req) =:= true of
                true ->
                    {ok, KeyValues, Req1} = cowboy_req:body_qs(Req),
                    ?DEBUG("POST Module ~p, KeyValues ~p~n", [Module, KeyValues]),
                    case erlang:function_exported(Module, execute_post, 2) of
                        true ->
                            Module:execute_post(KeyValues, Req1);
                        false ->
                            reply_misc:method_not_allowed(Req)
                    end;
                false ->
                    reply_misc:bad_request(<<"Missing body">>, Req)
            end;
        OtherMethod ->
            ?WARNING_MSG("Other Method ~ts~n", [OtherMethod]),
            reply_misc:method_not_allowed(Req)
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
