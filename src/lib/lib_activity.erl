-module(lib_activity).

-export([admin_accept/2,
         admin_reject/2,
         admin_delete/2]).

-include("common.hrl").
-include("define_user.hrl").
-include("define_activity.hrl").

admin_accept(ActivityId, Token) ->
    change_activity_status(ActivityId, Token, ?ACTIVITY_STATUS_ACCEPTED).

admin_reject(ActivityId, Token) ->
    change_activity_status(ActivityId, Token, ?ACTIVITY_STATUS_REJECTED).

change_activity_status(ActivityId, Token, Status) -> 
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
                        application_deadline = ApplicationDeadline
                       }=Activity} ->
                    ApplicationDeadlineTimeStamp = time_misc:db_datetime_to_timestamp(ApplicationDeadline),            
                    Now = time_misc:unixtime(),
                    if
                        ApplicationDeadlineTimeStamp =< Now ->
                            %% 活动已过期
                            ?FAIL(?INFO_ACTIVITY_APPLICATION_DEADLINE_EXPIRED);
                        true ->
                            db_activity:update(Activity#activity{
                                                 status = Status
                                                }),
                            ?FAIL(?INFO_OK)
                    end
            end
    end.

admin_delete(ActivityId, Token) ->
    case lib_user:is_admin_user(Token) of
        ?FAIL_REASON ->
            ?FAIL_REASON;
        ok ->
            delete(ActivityId),
            ?FAIL(?INFO_OK)
    end.
%%  tables `activity`, `user_activity_relation`, `activity_image_relation`, `image`, `comment`, `assessment` 
%% TODO image assessment activity_image_relation
delete(ActivityId) ->
    db_activity:delete_by_id(ActivityId),
    db_user_activity_relation:delete_by_activity_id(ActivityId),    
    db_comment:delete_by_activity_id(ActivityId).
    
