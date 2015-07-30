-module(lib_activity).

-export([admin_accept/2,
         admin_reject/2,
         admin_delete/2]).

-export([join/2, participants_update/3]).

-include("common.hrl").
-include("define_user.hrl").
-include("define_activity.hrl").
-include("db_notification.hrl").
-include("db_user_activity_relation.hrl").

admin_accept(ActivityId, Token) ->
    change_activity_status(ActivityId, Token, ?ACTIVITY_STATUS_ACCEPTED, last_accepted_time).

admin_reject(ActivityId, Token) ->
    change_activity_status(ActivityId, Token, ?ACTIVITY_STATUS_REJECTED, last_rejected_time).

change_activity_status(ActivityId, Token, Status, UpdateTimeStampField) -> 
    case lib_user:is_admin_user(Token) of
        ?FAIL_REASON ->
            ?FAIL_REASON;
        ok ->
            case db_activity:find(ActivityId) of
                ?FAIL_REASON ->
                    ?FAIL_REASON;
                {ok, #activity{
                        status = Status
                       }} ->
                    ?FAIL(?INFO_ACTIVITY_SAME_STATUS);
                {ok, #activity{
                        application_deadline = ApplicationDeadlineTimeStamp,
                        host_id = HostId
                       }=Activity} ->          
                    Now = time_misc:long_unixtime(),
                    if
                        ApplicationDeadlineTimeStamp =< Now ->
                            %% 活动已过期
                            ?FAIL(?INFO_ACTIVITY_APPLICATION_DEADLINE_EXPIRED);
                        true ->                            
                            db_activity:update([{UpdateTimeStampField, time_misc:long_unixtime()}, 
                                                {status, Status}], {id, '=', ActivityId}),
                            lib_notification:insert_and_push(notification(ActivityId, HostId, Status),
                                                             fun notification_pack/1),
                            begin_noti(Activity, Status),
                            ?FAIL(?INFO_OK)
                    end
            end
    end.

begin_noti(Activity, ?ACTIVITY_STATUS_ACCEPTED) ->    
    mod_activity_noti:begin_noti(Activity#activity.id, Activity#activity.begin_time);
begin_noti(_, _) -> 
    ingore.

notification(ActivityId, To, ?ACTIVITY_STATUS_ACCEPTED) ->
    #notification{
       cmd = ?S2C_ACTIVITY_ACCEPTED,
       activity_id = ActivityId,
       status = ?ACTIVITY_STATUS_ACCEPTED,
       %% content = <<"Your activity(id: <", 
       %%             (integer_to_binary(ActivityId))/binary,
       %%             ">) is accepted">>,
       to = To
      };
notification(ActivityId, To, ?ACTIVITY_STATUS_REJECTED) ->
    #notification{
       cmd = ?S2C_ACTIVITY_REJECTED,
       activity_id = ActivityId,
       status = ?ACTIVITY_STATUS_REJECTED,
       %% content = <<"Your activity(id: <", 
       %%             (integer_to_binary(ActivityId))/binary,
       %%             ">) is rejected">>,
       to = To
      };
notification(ActivityId, To, ?ACTIVITY_STATUS_DELETED) ->
     #notification{
       cmd = ?S2C_ACTIVITY_DELETED,
       activity_id = ActivityId,
       status = ?ACTIVITY_STATUS_DELETED,
       %% content = <<"Your activity(id: <", 
       %%             (integer_to_binary(ActivityId))/binary,
       %%             ">) is deleted">>,
       to = To
      }.

notification_pack(Notification) ->
    ?JSON([{id, Notification#notification.id},
           {activity_id, Notification#notification.activity_id},
           {status, Notification#notification.status}]).

%% content is 
admin_delete(ActivityId, Token) ->
    case lib_user:is_admin_user(Token) of
        ?FAIL_REASON ->
            ?FAIL_REASON;
        ok ->
            case db_activity:find(ActivityId) of
                ?FAIL_REASON ->
                    ?FAIL_REASON;
                {ok, #activity{
                        host_id = HostId
                       }} ->
                    lib_notification:insert_and_push(notification(ActivityId, HostId, ?ACTIVITY_STATUS_DELETED),
                                                             fun notification_pack/1),
                    delete(ActivityId),
                    ?FAIL(?INFO_OK)
            end
    end.
%%  tables `activity`, `user_activity_relation`, `activity_image_relation`, `image`, `comment`, `assessment` 
%% TODO image assessment activity_image_relation
delete(ActivityId) ->
    db_activity:delete_by_id(ActivityId),
    db_user_activity_relation:delete_by_activity_id(ActivityId),    
    db_comment:delete_by_activity_id(ActivityId).
    
-define(MAX_APPLIED, 500).

join(UserId, ActivityId) ->
    case db_activity:find(ActivityId) of
        ?FAIL_REASON ->
            ?FAIL_REASON;
        {ok, #activity{
                application_deadline = ApplicationDeadlineTimeStamp,
                status = Status,
                id = ActivityId,
                host_id= HostId,
                num_applied = NumApplied
               }} ->
            Now = time_misc:long_unixtime(),
            if
                Status =/= ?ACTIVITY_STATUS_ACCEPTED ->
                    ?FAIL(?INFO_ACTIVITY_NOT_ACCEPTED);
                ApplicationDeadlineTimeStamp =< Now ->
                    %% 活动已过期
                    ?FAIL(?INFO_ACTIVITY_APPLICATION_DEADLINE_EXPIRED);
                NumApplied + 1 > ?MAX_APPLIED ->
                    ?FAIL(?INFO_ACTIVITY_APPLIED_LIMIT);
                true ->
                    case db_user_activity_relation:user_activity_relation(UserId, ActivityId) of
                        {ok, [_]} ->
                            ?FAIL(?INFO_ACTIVITY_JOINED);
                        {ok, []} ->
                            UserActivityRelation = #user_activity_relation{
                                                      user_id = UserId,
                                                      activity_id = ActivityId,
                                                      relation = 1, %% 文档这么写的
                                                      generated_time = Now
                                                     },
                            case mysql_pool:transaction(hongkongresort, 
                                                        fun(Db) ->
                                                                db_user_activity_relation:insert(Db, UserActivityRelation),
                                                                db_activity:incr_num_applied(Db, ActivityId),
                                                                ok
                                                        end) of
                                {atomic, ok}  ->
                                    Notification0 = #notification{
                                                       cmd = ?S2C_ACTIVITY_JOIN,
                                                       activity_id = ActivityId,
                                                       relation = 1,
                                                       %% content = <<"@<", (lib_user:user_name(UserId))/binary, "> 報名參加活動 id<"/utf8,
                                                       %%             (integer_to_binary(ActivityId))/binary,
                                                       %%             ">">>,
                                                       from = UserId,
                                                       to = HostId
                                                      },
                                    lib_notification:insert_and_push(Notification0, 
                                                                     fun (Notification) ->
                                                                             ?JSON([{id, Notification#notification.id},
                                                                                    {activity_id, Notification#notification.activity_id},
                                                                                    {from, Notification#notification.from}])
                                                                     end),
                                    ?FAIL(?INFO_OK);
                                {aborted, _} ->
                                    ?FAIL(?INFO_DB_ERROR)
                            end
                    end
            end
    end.

-define(MAX_SELECTED, 250).

participants_update(UserId, ActivityId, Bundle) ->    
    case db_activity:find(ActivityId) of
        ?FAIL_REASON ->
            ?FAIL_REASON;
        {ok, #activity{
                begin_time = BeginTime,
                status = Status,
                id = ActivityId,
                host_id= HostId,
                num_selected = NumSelected
               }} ->
            Now = time_misc:long_unixtime(),
            if
                Status =/= ?ACTIVITY_STATUS_ACCEPTED ->
                    ?FAIL(?INFO_ACTIVITY_NOT_ACCEPTED);
                HostId =/= UserId ->
                    ?FAIL(?INFO_ACTIVITY_NOT_HOST);
                BeginTime =< Now ->
                    %% 活动已开始
                    ?FAIL(?INFO_ACTIVITY_HAS_BEGUN);
                true ->
                    AcceptedUserIds = db_user_activity_relation:accepted_user_ids(ActivityId),
                    case Bundle -- AcceptedUserIds of
                        [] ->
                            if 
                                length(AcceptedUserIds) + NumSelected > ?MAX_SELECTED ->
                                    ?FAIL(?INFO_ACTIVITY_SELECTED_LIMIT);
                                true ->
                                    case mysql_pool:transaction(hongkongresort, 
                                                                fun(Db) ->
                                                                        db_user_activity_relation:update_select_relation(Db, ActivityId, Bundle),
                                                                        db_activity:update_select_count(Db, ActivityId, length(Bundle)),
                                                                        ok
                                                                end) of
                                        {atomic, ok}  ->
                                            Notification0 = #notification{
                                                               cmd = ?S2C_PARTICIPANTS_UPDATE,
                                                               activity_id = ActivityId,
                                                               relation = 2,
                                                               from = HostId
                                                              },
                                            [lib_notification:insert_and_push(Notification0#notification{
                                                                                to = To
                                                                               }, 
                                                                              fun (Notification) ->
                                                                                      ?JSON([{id, Notification#notification.id},
                                                                                             {activity_id, Notification#notification.activity_id},
                                                                                             {from, Notification#notification.from}])
                                                                              end) || To <- Bundle],
                                            ?FAIL(?INFO_OK);
                                        {aborted, _} ->
                                            ?FAIL(?INFO_DB_ERROR)
                                    end
                            end;
                        _ ->
                           ?FAIL(?INFO_PARAMETER_ERROR)
                    end
            end
    end.
