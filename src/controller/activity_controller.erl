-module(activity_controller).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-export([execute_get/3]).

-include("db_activity.hrl").
-include("db_user.hrl").
-include("db_image.hrl").
-include("db_user_activity_relation.hrl").

-include("common.hrl").
-include("define_info_0.hrl").
-define(ACTION_QUERY, [<<"query">>]).

-define(PARAM_PAGE, <<"page">>).
-define(PARAM_NUM_ITEMS, <<"num_items">>).
-define(PARAM_ORDER_KEY, <<"order_key">>).
-define(PARAM_ORIENTATION, <<"orientation">>). 
-define(PARAM_TOKEN, <<"token">>). 
     

init(_Type, Req, _Env) ->
	{ok, Req, undefined}.

handle(Req, State) ->
    GetParameter = [{?ACTION_QUERY, [{?PARAM_PAGE, int, required},
                                     {?PARAM_NUM_ITEMS, int, required},
                                     {?PARAM_ORDER_KEY, binary, required},
                                     {?PARAM_ORIENTATION, int, required},
                                     {?PARAM_TOKEN, binary, optional} %%token在业务层没用的，被解析转成UserId传上来
                                    ]
                    }],
    Req1 = controller_helper:execute(?MODULE, Req, [{get_parameter, GetParameter}]),
	{ok, Req1, State}.

%% /el/activity/query2?page=1&num_items=10&order_key=xxx&orientation=1&token=0a029a1451b987fd3401f3820ec5139a     
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
                    Response = ?JSON([{count, 0}, {activities, ActivityResponse}]),
                    {json, Response}
            end
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

