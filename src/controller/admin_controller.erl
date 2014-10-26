-module(admin_controller).

-export([init/2]).
-export([terminate/3]).

-export([execute_post/3]).

-include("db_activity.hrl").
-include("db_user.hrl").
-include("db_image.hrl").
-include("db_user_activity_relation.hrl").

-include("common.hrl").
-include("define_info_0.hrl").
-define(ACTION_ACTIVITY_ACCEPT, [<<"activity">>, <<"accept">>]).
-define(ACTION_ACTIVITY_REJECT, [<<"activity">>, <<"reject">>]).
-define(ACTION_ACTIVITY_DELETE, [<<"activity">>, <<"delete">>]).

-define(PARAM_ACTIVITY_ID, <<"activity_id">>).
-define(PARAM_TOKEN, <<"token">>). 

-define(PARAM_NUM_ITEMS, <<"num_items">>).
-define(PARAM_ORDER_KEY, <<"order_key">>).
-define(PARAM_ORIENTATION, <<"orientation">>). 
     
init(Req, _Opts) ->
    PostParameter = [{?ACTION_ACTIVITY_ACCEPT, [{?PARAM_ACTIVITY_ID, int, required},
                                                {?PARAM_TOKEN, binary, optional}
                                               ]
                     },
                     {?ACTION_ACTIVITY_REJECT, [{?PARAM_ACTIVITY_ID, int, required},
                                                {?PARAM_TOKEN, binary, optional}
                                               ]
                     },
                     {?ACTION_ACTIVITY_DELETE, [{?PARAM_ACTIVITY_ID, int, required},
                                                {?PARAM_TOKEN, binary, optional}
                                               ]
                     }],
    ControllerOpts = [{post_parameter, PostParameter}],
	{controller_helper, Req, ControllerOpts}.

%% /el/admin/activity/accept
%% activity_id=7&token=0b8e47d16124c77b1a406048967757e4
execute_post(?ACTION_ACTIVITY_ACCEPT, [ActivityId, Token], _Req) ->   
    lib_activity:admin_accept(ActivityId, Token);
%% /el/admin/activity/reject
%% activity_id=7&token=0b8e47d16124c77b1a406048967757e4
execute_post(?ACTION_ACTIVITY_REJECT, [ActivityId, Token], _Req) ->   
    lib_activity:admin_reject(ActivityId, Token);
%% /el/admin/activity/delete
%% activity_id=7&token=0b8e47d16124c77b1a406048967757e4
execute_post(?ACTION_ACTIVITY_DELETE, [ActivityId, Token], _Req) ->   
    lib_activity:admin_delete(ActivityId, Token).

terminate(_Reason, _Req, _State) ->
	ok.

