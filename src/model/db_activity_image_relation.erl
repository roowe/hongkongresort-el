-module(db_activity_image_relation).

-export([activity_image_relation/1]).

-include("db_activity_image_relation.hrl").
-include("define_mysql.hrl").

-define(TABLE_CONF, #record_mysql_info{
                       db_pool = hongkongresort,
                       table_name = activity_image_relation,
                       record_name = activity_image_relation,
                       fields = record_info(fields, activity_image_relation)                      
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
activity_image_relation(ActivityId) ->
    db_mysql_base:select(?TABLE_CONF, {activity_id, '=', ActivityId}).



