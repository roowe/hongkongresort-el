-module(comment_controller).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-export([execute_get/3]).

-include("common.hrl").
-include("db_activity.hrl").
-include("define_info_0.hrl").

init(_Type, Req, _Env) ->
	{ok, Req, undefined}.

handle(Req, State) ->
    Req1 = controller_helper:execute(?MODULE, Req),
	{ok, Req1, State}.

%%  /el/activity/query/page=1&num_items=10
execute_get(<<"query">>, KeyValues, Req) ->
    %% page, num_items
    PageBin = proplists:get_value(<<"page">>, KeyValues),
    NumItemsBin = proplists:get_value(<<"num_items">>, KeyValues),
    case cowboy_misc:not_qs_empty([PageBin, NumItemsBin]) of
        false ->
            reply_misc:ok_reply(json, 
                                {[{ret, ?INFO_PARAMETER_MISS}]},
                                Req);
        true ->
            Page = erlang:binary_to_integer(PageBin),
            NumItems = erlang:binary_to_integer(NumItemsBin),
            Offset = (Page-1)*NumItems,
            {ok, List} = db_activity:page(Offset, NumItems),
            Response = [{to_client_activity(Activity)} || Activity <- List],
            reply_misc:ok_reply(json, 
                                {[{ret, ?INFO_OK},
                                  {response, Response}]},
                                Req)
    end;
execute_get(Action, KeyValues, Req) ->
    ?WARNING_MSG("Action ~p, KeyValues ~p~n", [Action, KeyValues]),
    reply_misc:ok_reply(json, 
                        {[{ret, ?INFO_ACTION_MISS}]},
                        Req).

   

to_client_activity(Activity) ->
    [{id, Activity#activity.id},
     {title, Activity#activity.title},
     {content, Activity#activity.content}].

    
terminate(_Reason, _Req, _State) ->
	ok.

