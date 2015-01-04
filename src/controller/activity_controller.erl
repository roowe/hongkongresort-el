-module(activity_controller).

-export([init/2]).
-export([terminate/3]).

-export([execute_get/3,
         execute_post/3]).

-include("db_activity.hrl").
-include("db_user.hrl").
-include("db_image.hrl").
-include("db_user_activity_relation.hrl").

-include("common.hrl").
-include("define_info_0.hrl").
-define(ACTION_QUERY, [<<"query">>]).
-define(ACTION_JOIN, [<<"join">>]).
-define(ACTION_PARTICIPANTS_UPDATE, [<<"participants">>, <<"update">>]).

-define(PARAM_PAGE, <<"page">>).
-define(PARAM_NUM_ITEMS, <<"num_items">>).
-define(PARAM_ORDER_KEY, <<"order_key">>).
-define(PARAM_ORIENTATION, <<"orientation">>). 
-define(PARAM_TOKEN, <<"token">>). 
-define(PARAM_ACTIVITY_ID, <<"activity_id">>). 
-define(PARAM_BUNDLE, <<"bundle">>).

init(Req, _Opts) ->
    GetParameter = [{?ACTION_QUERY, [{?PARAM_PAGE, int, required},
                                     {?PARAM_NUM_ITEMS, int, required},
                                     {?PARAM_ORDER_KEY, binary, required},
                                     {?PARAM_ORIENTATION, int, required},
                                     {?PARAM_TOKEN, binary, optional}
                                    ]
                    }],
    PostParameter = [{?ACTION_JOIN, [{?PARAM_TOKEN, binary, required},
                                     {?PARAM_ACTIVITY_ID, int, required}]
                     },
                     {?ACTION_PARTICIPANTS_UPDATE, [{?PARAM_TOKEN, binary, required},
                                                    {?PARAM_ACTIVITY_ID, int, required},
                                                    {?PARAM_BUNDLE, json, required}]}
                    ],
    ControllerOpts = [{get_parameter, GetParameter}, {post_parameter, PostParameter}],
	{controller_helper, Req, ControllerOpts}.

%% /el/activity/query?page=1&num_items=10&order_key=id&orientation=1&token=0a029a1451b987fd3401f3820ec5139a     
execute_get(?ACTION_QUERY, [Page, NumItems, OrderKey, Orientation, Token], _Req) ->   
    %%  # {"id", "begin_time", "application_deadline", "created_time"}
    case allow_order_key(OrderKey) of
        false ->
            {fail, ?INFO_PARAMETER_ERROR};
        true ->
            {ok, List} = db_activity:page(Page, NumItems, binary_to_atom(OrderKey, utf8), Orientation),
            case lib_user:user_id_by_token(Token) of
                {fail, Reason} ->
                    {fail, Reason};
                {ok, UserId} ->
                    ActivityResponse = [begin
                                            Images = lib_image:activity_images(Id),
                                            HostUser = lib_user:user(HostId),
                                            Relation = relation(UserId, Id),
                                            pack(Activity, Images, HostUser, Relation)
                                        end || #activity{
                                                  id = Id,
                                                  host_id = HostId
                                                 } = Activity <- List],
                    Response = ?JSON([{count, 0}, {page, Page}, {activities, ActivityResponse}]),
                    {json, Response}
            end
    end.
%% /el/activity/join
%% activity_id=4&token=0373d498650a3ad4e4cd561b7221e954
execute_post(?ACTION_JOIN, [Token, ActivityId], _Req) -> 
    case lib_user:user_id_by_token(Token) of
        ?FAIL_REASON ->
            ?FAIL_REASON;
        {ok, UserId} ->
            lib_activity:join(UserId, ActivityId)
    end;
%% /el/activity/participants/update
%% token=0373d498650a3ad4e4cd561b7221e954&&activity_id=1&&bundle="[11,12,13]"
execute_post(?ACTION_PARTICIPANTS_UPDATE, [Token, ActivityId, Bundle], _Req) -> 
    case lib_user:user_id_by_token(Token) of
        ?FAIL_REASON ->
            ?FAIL_REASON;
        {ok, UserId} ->
            lib_activity:participants_update(UserId, ActivityId, Bundle)
    end.

allow_order_key(<<"id">>) ->
    true;
allow_order_key(<<"begin_time">>) ->
    true;
allow_order_key(<<"application_deadline">>) ->
    true;
allow_order_key(<<"created_time">>) ->
    true;
allow_order_key(_) ->
    false.

relation(undefined, _) ->
    undefined;
relation(UserId, ActivityId) ->
    case db_user_activity_relation:user_activity_relation(UserId, ActivityId) of
        {ok, [#user_activity_relation{
                 relation = Relation
                }]} ->
            Relation;
        _ ->
            undefined
    end.

-define(activity_kv(Field), {Field, Activity#activity.Field}).   
-define(activity_kv(Field, F), {Field, F(Activity#activity.Field)}).   

pack(Activity, Images, HostUser, Relation) ->   
    Base = [?activity_kv(id),
            ?activity_kv(title),
            ?activity_kv(content),
            ?activity_kv(created_time),
            ?activity_kv(begin_time),
            ?activity_kv(application_deadline),
            ?activity_kv(capacity),
            ?activity_kv(status)],
    AddHost = if
                  HostUser =:= [] ->
                      Base;
                  true ->
                      [{host, ?JSON([{id, HostUser#user.id},
                                     {email, HostUser#user.email},
                                     {name, HostUser#user.name}])} | Base]
              end,
    AddRelation = if
                      Relation =:= undefined ->
                          AddHost;
                      true ->
                          [{relation, Relation}|AddHost]
                  end,
    AddImages = if
                    Images =:= [] ->
                        AddRelation;
                    true ->
                        [{images, [?JSON([{id, Id},
                                          {url, URL}]) || #image{
                                                            id = Id,
                                                             url = URL
                                                            } <- Images]} | AddRelation]
                end,
    ?JSON(AddImages).

    
terminate(_Reason, _Req, _State) ->
	ok.

