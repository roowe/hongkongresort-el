-module(db_user).

-export([user/1]).

-export([incr_unread_count/2]).

-include("db_user.hrl").
-include("define_mysql.hrl").

-define(TABLE_CONF, #record_mysql_info{
                       db_pool = hongkongresort,
                       table_name = user,
                       record_name = user,
                       fields = record_info(fields, player)
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
user(Id) ->
    db_mysql_base:select(?TABLE_CONF, {id, '=', Id}).

incr_unread_count(Id, Op) ->
    db_mysql_base:update(?TABLE_CONF,
                         [{unread_count, {unread_count,Op, 1}}], 
                         {id,'=', Id}).
