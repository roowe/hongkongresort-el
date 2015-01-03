-module(lib_activity).

-export([admin_accept/2,
         admin_reject/2,
         admin_delete/2]).

-export([join/2]).

-include("common.hrl").
-include("define_user.hrl").
-include("define_activity.hrl").
-include("db_notification.hrl").
-include("db_user_activity_relation.hrl").

admin_accept(ActivityId, Token) ->
    change_activity_status(ActivityId, Token, ?ACTIVITY_STATUS_ACCEPTED, #activity.last_accepted_time).

admin_reject(ActivityId, Token) ->
    change_activity_status(ActivityId, Token, ?ACTIVITY_STATUS_REJECTED, #activity.last_rejected_time).

change_activity_status(ActivityId, Token, Status, UpdateTimeStampPos) -> 
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
                            db_activity:update(setelement(UpdateTimeStampPos, 
                                                          Activity#activity{
                                                            status = Status
                                                           }, 
                                                          time_misc:long_unixtime())),
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
       content = <<"Your activity(id: <", 
                   (integer_to_binary(ActivityId))/binary,
                   ">) is accepted">>,
       to = To
      };
notification(ActivityId, To, ?ACTIVITY_STATUS_REJECTED) ->
    #notification{
       cmd = ?S2C_ACTIVITY_REJECTED,
       activity_id = ActivityId,
       status = ?ACTIVITY_STATUS_REJECTED,
       content = <<"Your activity(id: <", 
                   (integer_to_binary(ActivityId))/binary,
                   ">) is rejected">>,
       to = To
      };
notification(ActivityId, To, ?ACTIVITY_STATUS_DELETED) ->
     #notification{
       cmd = ?S2C_ACTIVITY_DELETED,
       activity_id = ActivityId,
       status = ?ACTIVITY_STATUS_DELETED,
       content = <<"Your activity(id: <", 
                   (integer_to_binary(ActivityId))/binary,
                   ">) is deleted">>,
       to = To
      }.

notification_pack(Notification) ->
    ?JSON([{id, Notification#notification.id},
           {activity_id, Notification#notification.activity_id},
           {status, Notification#notification.status},
           {content, Notification#notification.content}]).

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
    
join(UserId, ActivityId) ->
    case db_activity:find(ActivityId) of
        ?FAIL_REASON ->
            ?FAIL_REASON;
        {ok, #activity{
                application_deadline = ApplicationDeadlineTimeStamp,
                status = Status,
                id = ActivityId,
                host_id= HostId
               }} ->
            Now = time_misc:long_unixtime(),
            if
                Status =/= ?ACTIVITY_STATUS_ACCEPTED ->
                    ?FAIL(?INFO_ACTIVITY_STATUS_NOT_ACCEPTED);
                ApplicationDeadlineTimeStamp =< Now ->
                    %% 活动已过期
                    ?FAIL(?INFO_ACTIVITY_APPLICATION_DEADLINE_EXPIRED);
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
                            db_user_activity_relation:insert(UserActivityRelation),
                            db_activity:incr_num_applied(ActivityId),

                            Notification0 = #notification{
                                              cmd = ?S2C_ACTIVITY_JOIN,
                                              activity_id = ActivityId,
                                              relation = 1,
                                              content = <<"@<", (lib_user:user_name(UserId))/binary, "> 報名參加活動 id<"/utf8,
                                                          (integer_to_binary(ActivityId))/binary,
                                                          ">">>,
                                              from = UserId,
                                              to = HostId
                                             },
                            lib_notification:insert_and_push(Notification0, 
                                                             fun (Notification) ->
                                                                     ?JSON([{id, Notification#notification.id},
                                                                            {activity_id, Notification#notification.activity_id},
                                                                            {from, Notification#notification.from},
                                                                            {content, Notification#notification.content}])
                                                             end),
                            ?FAIL(?INFO_OK)
                    end
            end
    end.
