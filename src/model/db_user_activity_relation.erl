-module(db_user_activity_relation).

-export([user_activity_relation/2]).
-export([delete_by_activity_id/1]).

-include("db_user_activity_relation.hrl").
-include("define_mysql.hrl").

-define(TABLE_CONF, #record_mysql_info{
                       db_pool = hongkongresort,
                       table_name = user_activity_relation,
                       record_name = user_activity_relation,
                       fields = record_info(fields, user_activity_relation)
                      }).

%% --------------------通用代码--------------------
-export([update/1, delete/1, insert/1, r_list_insert_withnot_id/1, r_list_insert_with_id/1]).

update(Record)->
    db_mysql_base:r_update(?TABLE_CONF, Record).

delete(RecordOrList) ->
    db_mysql_base:r_delete(?TABLE_CONF, RecordOrList).

insert(Record) ->
    db_mysql_base:r_insert(?TABLE_CONF, Record).

r_list_insert_withnot_id(List) ->
    db_mysql_base:r_list_insert_withnot_id(?TABLE_CONF, List).

r_list_insert_with_id(List) ->
    db_mysql_base:r_list_insert_with_id(?TABLE_CONF, List).

%% ----------------------------------------
user_activity_relation(UserId, ActivityId) ->
    db_mysql_base:select(?TABLE_CONF, {{user_id, '=', UserId}, 'and', {activity_id, '=', ActivityId}}).

delete_by_activity_id(ActivityId) ->
    db_mysql_base:delete(?TABLE_CONF, {activity_id, '=', ActivityId}).


