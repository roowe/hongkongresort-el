-module(activity_controller).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-export([execute_get/3]).

-include("common.hrl").
-include("db_activity.hrl").
-include("define_info_0.hrl").
-define(ACTION_QUERY, [<<"query">>]).
-define(PARAM_PAGE, <<"page">>).
-define(PARAM_NUM_ITEMS, <<"num_items">>).


init(_Type, Req, _Env) ->
	{ok, Req, undefined}.

handle(Req, State) ->
    GetParameter = [{?ACTION_QUERY, [{?PARAM_PAGE, int, required},
                                     {?PARAM_NUM_ITEMS, int, required}]
                    }],
    Req1 = controller_helper:execute(?MODULE, Req, [{get_parameter, GetParameter}]),
	{ok, Req1, State}.

%%  /el/activity/query?page=1&num_items=10
execute_get(?ACTION_QUERY, [Page, NumItems], Req) ->    
    {ok, List} = db_activity:page(Page, NumItems),
    Response = [?JSON(to_client_data(Activity)) || Activity <- List],
    reply_misc:ok_reply(json, 
                        {[{ret, ?INFO_OK},
                          {response, Response}]},
                        Req).
    
-define(activity_kv(Field), {Field, Activity#activity.Field}).   
to_client_data(Activity) ->
    [?activity_kv(id),
     ?activity_kv(title),
     ?activity_kv(content)].

    
terminate(_Reason, _Req, _State) ->
	ok.

