-module(db_activity).

-export([page/4]).
-export([find/1]).
-export([delete_by_id/1]).
-export([not_begin_accept_activity_info/0]).

-export([incr_num_applied/2, update_select_count/3]).

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
-export([insert/1]).
-export([update/2]).

insert(Record) ->
    db_mysql_base:r_insert(?TABLE_CONF, Record).

%% ----------------------------------------


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
    case db_mysql_base:select(?TABLE_CONF, [id, begin_time], {{status, '=', ?ACTIVITY_STATUS_ACCEPTED}, 'and', {begin_time, '>', Now}}, undefined) of
        {ok, [#mysql_resultset{
                 rows = Rows
                }]} ->
            Rows;
        _ ->
            []
    end.

incr_num_applied(Db, Id) ->
    update(Db, [{num_applied, {num_applied, '+', 1}}], {id, '=', Id}).

update_select_count(Db, Id, SelectCount) ->
    update(Db, [{num_applied, {num_applied, '-', SelectCount}}, 
            {num_selected, {num_selected, '+', SelectCount}}], {id, '=', Id}).

update(UpdateClause, WhereClause) ->
    {ok, _} = db_mysql_base:update(?TABLE_CONF, UpdateClause, WhereClause).

update(Db, UpdateClause, WhereClause) ->
    {ok, _} = db_mysql_base:update(?TABLE_CONF#record_mysql_info{
                                      db_pool = Db
                                     }, UpdateClause, WhereClause).
