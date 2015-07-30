-module(db_user_activity_relation).

-export([user_activity_relation/2]).
-export([delete_by_activity_id/1]).

-export([sign_up_user_ids/1, accepted_user_ids/1]).

-export([update_select_relation/3]).

-include("db_user_activity_relation.hrl").
-include("define_mysql.hrl").

-define(TABLE_CONF, #record_mysql_info{
                       db_pool = hongkongresort,
                       table_name = user_activity_relation,
                       record_name = user_activity_relation,
                       fields = record_info(fields, user_activity_relation)
                      }).

%% --------------------通用代码--------------------
-export([update/1, delete/1, insert/2, r_list_insert_withnot_id/1, r_list_insert_with_id/1]).

update(Record)->
    db_mysql_base:r_update(?TABLE_CONF, Record).

delete(RecordOrList) ->
    db_mysql_base:r_delete(?TABLE_CONF, RecordOrList).

insert(Db, Record) ->
    {ok, _} = db_mysql_base:r_insert(?TABLE_CONF#record_mysql_info{
                                        db_pool = Db
                                       }, Record).

r_list_insert_withnot_id(List) ->
    db_mysql_base:r_list_insert_withnot_id(?TABLE_CONF, List).

r_list_insert_with_id(List) ->
    db_mysql_base:r_list_insert_with_id(?TABLE_CONF, List).

%% ----------------------------------------
user_activity_relation(UserId, ActivityId) ->
    db_mysql_base:select(?TABLE_CONF, {{user_id, '=', UserId}, 'and', {activity_id, '=', ActivityId}}).

delete_by_activity_id(ActivityId) ->
    db_mysql_base:delete(?TABLE_CONF, {activity_id, '=', ActivityId}).

%% 操作成功後需要設定timer在begin_time的時刻向所有被選中的報名者(select `user_id` from `user_activity_relation` where `activity_id`=<from request data> and  `relation`=2) 發送以下消息（push via web socket and save it to db）: 
sign_up_user_ids(ActivityId) ->
    select_by_relation(ActivityId, 2).

accepted_user_ids(ActivityId) ->
    select_by_relation(ActivityId, 1).

select_by_relation(ActivityId, Relation) ->
    {ok, #mysql_resultset{
            rows = List
           }} = db_mysql_base:select(?TABLE_CONF, user_id, {{activity_id, '=', ActivityId}, 'and', {relation, '=', Relation}}, undefined),
    lists:append(List).
    
%% UPDATE  `user_activity_relation` SET  relation=2 WHERE activity_id=1 AND user_id in (10, 11)
update_select_relation(Db, ActivityId, UserIds) ->
    Now = time_misc:long_unixtime(),
    {ok, _} = db_mysql_base:update(?TABLE_CONF#record_mysql_info{
                                      db_pool = Db
                                     }, [{relation, 2}, {last_selected_time, Now}], {{activity_id, '=', ActivityId}, 'and', {user_id, in, UserIds}}).


