-module(db_activity).

-export([page/2]).

-include("db_activity.hrl").
-include("define_mysql.hrl").

-define(TABLE_CONF, #record_mysql_info{
                       db_pool = hongkongresort,
                       table_name = activity,
                       record_name = activity,
                       fields = record_info(fields, activity)
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

test_insert() ->
    GenFun = fun(Id) ->
                     #activity{
                        title = <<"活动"/utf8, (integer_to_binary(Id))/binary>>,
                        content = <<"活动内容长长长长长长长长长长"/utf8, (integer_to_binary(Id))/binary>>,
                        host_id = 12
                       }
             end,
    r_list_insert_withnot_id([GenFun(Id) || Id <- lists:seq(1,10)]).


page(Offset, Num) ->
    db_mysql_base:select(?TABLE_CONF, undefined, [{limit, Offset, Num}]).