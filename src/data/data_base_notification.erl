-module(data_base_notification).

-export([zh_hk/1]).

-include("common.hrl").
-include("define_ws_cmd.hrl").
-include("db_notification.hrl").
-include("define_activity.hrl").

zh_hk(Notification) ->
    unicode:characters_to_binary(zh_hk2(Notification)).

zh_hk2(#notification{
          cmd = ?S2C_COMMENT_SUBMIT,
          activity_id = ActivityId
         }) ->
    Format = "有人對你\"~ts\"的活動感興趣并已留言！快去回覆TA",
    io_lib:format(Format, [short_activity_name(ActivityId)]);
zh_hk2(#notification{
          cmd = ?S2C_COMMENT_SUB_SUBMIT,
          from = FromId,
          activity_id = ActivityId
         }) ->
    Format = "~ts回應了你在\"~ts\"的提問",
    io_lib:format(Format, [lib_user:user_name(FromId), short_activity_name(ActivityId)]);
zh_hk2(#notification{
          cmd = ?S2C_ACTIVITY_ACCEPTED,
          activity_id = ActivityId
         }) ->
    Format = "你\"~ts\"的活動上线了",
    io_lib:format(Format, [short_activity_name(ActivityId)]);
zh_hk2(#notification{
          cmd = ?S2C_ACTIVITY_REJECTED, 
          activity_id = ActivityId
         }) ->
    Format = "很遺憾，你\"~ts\"的活動未通過審核，請修改并再次提交",
    io_lib:format(Format, [short_activity_name(ActivityId)]);
zh_hk2(#notification{
          cmd = ?S2C_ACTIVITY_DELETED, 
          activity_id = ActivityId
         }) ->
    Format = "你\"~ts\"的活動被管理員刪除了，不服就找TA理論吧！",
    io_lib:format(Format, [short_activity_name(ActivityId)]);
zh_hk2(#notification{
          cmd = ?S2C_PARTICIPANTS_UPDATE, 
          activity_id = ActivityId, 
          from = FromId
         }) ->
    Format = "恭喜你被\"~ts\"臨幸，成功參加\"~ts\"",
    io_lib:format(Format, [lib_user:user_name(FromId), short_activity_name(ActivityId)]);
zh_hk2(#notification{
          cmd = ?S2C_ACTIVITY_JOIN, 
          activity_id = ActivityId, 
          from = FromId
         }) ->
    Format = "恭喜\"~ts\"報名參加你\"~ts\"的活動，赶快屁颠屁颠地去確認吧！",
    io_lib:format(Format, [lib_user:user_name(FromId), short_activity_name(ActivityId)]).

short_activity_name(Id) ->
    case db_activity:find(Id) of
        ?FAIL_REASON ->
            <<"unknown">>;
        {ok, #activity{
                title = Name
               }} ->
            UnicodeName = unicode:characters_to_list(Name),
            Len = length(UnicodeName),
            if
                Len =< 6 ->
                    Name;
                true ->
                    unicode:characters_to_binary(lists:sublist(UnicodeName, 6) ++ "...")
            end
    end.
    
%% test() ->
%%     io:format("~w~n", [zh_hk(?S2C_COMMENT_SUBMIT, [<<"超级长超级长我要省略"/utf8>>])]).
