-module(db_activity).

-export([page/4]).
-export([find/1]).
-export([delete_by_id/1]).
-export([not_begin_accept_activity_info/0]).

-export([incr_num_applied/1, update_select_count/2]).

-include("db_activity.hrl").
-include("define_mysql.hrl").
-include("define_info_0.hrl").
-include("define_info_3.hrl").
-include("common.hrl").
-include("define_activity.hrl").

-define(TABLE_CONF, #record_mysql_info{
                       db_pool = hongkongresort,
                       table_name = activity,
                       record_name = activity,
                       fields = record_info(fields, activity)
                      }).


%% --------------------通用代码--------------------
-export([update/1, delete/1, insert/1, r_list_insert_withnot_id/1, r_list_insert_with_id/1]).

update(Record)->
    {ok, _} = db_mysql_base:r_update(?TABLE_CONF, Record).

delete(RecordOrList) ->
    {ok, _} = db_mysql_base:r_delete(?TABLE_CONF, RecordOrList).

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


page(Page, Num, OrderKey, Orientation0) ->
    Orientation = mysql_misc:orientation(Orientation0),
    Offset = mysql_misc:offset(Page, Num),
    db_mysql_base:select(?TABLE_CONF, undefined, [{order_by, {OrderKey, Orientation}},
                                                  {limit, Offset, Num}]).

find(Id) ->
    case db_mysql_base:select(?TABLE_CONF, {id, '=', Id}) of
        {ok, [Activity]} ->
            {ok, Activity};
        {ok, []} ->
            {fail, ?INFO_ACTIVITY_NOT_FOUND};
        {error, Error} ->
            ?WARNING_MSG("DB Error ~p~n", [Error]),
            {fail, ?INFO_DB_ERROR}
    end.

delete_by_id(Id) ->
    db_mysql_base:delete(?TABLE_CONF, {id, '=', Id}).

not_begin_accept_activity_info() ->
    Now = time_misc:long_unixtime(),
    db_mysql_base:select(?TABLE_CONF, [id, begin_time], {{status, '=', ?ACTIVITY_STATUS_ACCEPTED}, 'and', {begin_time, '>', Now}}, undefined).

incr_num_applied(Id) ->
    {ok, _} = db_mysql_base:update(?TABLE_CONF, [{num_applied, {num_applied, '+', 1}}], {id, '=', Id}).

update_select_count(Id, SelectCount) ->
    {ok, _} = db_mysql_base:update(?TABLE_CONF, [{num_applied, {num_applied, '-', SelectCount}}, 
                                                 {num_selected, {num_selected, '+', SelectCount}}], {id, '=', Id}).
