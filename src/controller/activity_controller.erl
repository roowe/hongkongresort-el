-module(activity_controller).

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
    GetParameter = [{<<"query">>, [{<<"page">>, int, required},
                                   {<<"num_items">>, int, required}]
                    }],
    Req1 = controller_helper:execute(?MODULE, Req, [{get_parameter, GetParameter}]),
	{ok, Req1, State}.

%%  /el/activity/query/page=1&num_items=10
execute_get(<<"query">>, [Page, NumItems], Req) ->    
    Offset = (Page-1)*NumItems,
    {ok, List} = db_activity:page(Offset, NumItems),
    Response = [{to_client_activity(Activity)} || Activity <- List],
    reply_misc:ok_reply(json, 
                        {[{ret, ?INFO_OK},
                          {response, Response}]},
                        Req).
    
   
to_client_activity(Activity) ->
    [{id, Activity#activity.id},
     {title, Activity#activity.title},
     {content, Activity#activity.content}].

    
terminate(_Reason, _Req, _State) ->
	ok.

