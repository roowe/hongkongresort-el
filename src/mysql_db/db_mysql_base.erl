-module(db_mysql_base).

-export([frag_select/2, frag_select/3]).

-export([select/2, select/3, select/4,
         update/3, delete/2]).

-export([db_run_rows/2]).

-export([
         r_update/2,
         r_delete/2,
         r_insert/2, r_replace/2,
         r_list_insert_withnot_id/2,
         r_list_insert_with_id/2,
         r_list_replace/2
        ]).

-export([run_affected/2, run_rows/2, run_rows/3]).

-export([as_record/3, as_record/4]).


-include("define_mysql.hrl").
-include("common.hrl").

select(RecordMysqlInfo, WhereClause) -> 
    select(RecordMysqlInfo, WhereClause, undefined).

select(#record_mysql_info{
          db_pool = DbPool,
          table_name = TableName,
          record_name = RecordName,
          out_db_hook = OutDbHook,
          fields = Fields
         }, WhereClause, Extras) ->
    SQL = iolist_to_binary(erl_mysql:select('*', TableName, WhereClause, Extras)),
    %% ?DEBUG("SQL ~ts~n", [SQL]),
    run_rows(DbPool, SQL,
             fun(Result) ->
                     as_record(Result, RecordName, Fields, OutDbHook)
             end).

select(#record_mysql_info{
          db_pool = DbPool,
          table_name = TableName
         }, Fields, WhereClause, Extras) ->
    SQL = iolist_to_binary(erl_mysql:select(Fields, TableName, WhereClause, Extras)),
    %% ?DEBUG("SQL ~ts~n", [SQL]),
    run_rows(DbPool, SQL).

frag_select(RecordMysqlInfo, WhereClause) -> 
    frag_select(RecordMysqlInfo, WhereClause, undefined).

frag_select(#record_mysql_info{
               table_name = TableNameList
              }=RecordMysqlInfo, WhereClause, Extras) 
  when is_list(TableNameList) ->
%    ?DEBUG("TableNameList ~p~n", [TableNameList]),
    lists:foldl(fun(TableName, Acc) ->
                        case select(RecordMysqlInfo#record_mysql_info{
                                      table_name = TableName
                                     }, WhereClause, Extras) of
                            {ok, List} ->
                                List ++ Acc;
                            {error, Error} ->
                                ?DEBUG("frag_select Error ~p~n", [Error]),
                                Acc
                        end
                end, [], TableNameList).

update(#record_mysql_info{
          db_pool = DbPool,
          table_name = TableName
         }, UpdateClause, WhereClause) ->
    SQL = iolist_to_binary(erl_mysql:update(TableName, UpdateClause, WhereClause)),
    %% ?DEBUG("SQL ~ts~n", [SQL]),
    run_affected(DbPool, SQL).

delete(#record_mysql_info{
          db_pool = DbPool,
          table_name = TableName
         }, WhereClause) ->
    SQL = iolist_to_binary(erl_mysql:delete(TableName, WhereClause)),
    run_affected(DbPool, SQL).

%% 下面带r的是，是针对record的接口
%% 返回值{ok, Record}，会自动处理，需不需要加上insert_id
%% 或者{error, Result}
r_insert(RecordMysqlInfo, Record) when is_tuple(Record)->
    r_insert2(RecordMysqlInfo, Record, insert).

r_replace(RecordMysqlInfo, Record) when is_tuple(Record)->
    r_insert2(RecordMysqlInfo, Record, replace).

r_insert2(#record_mysql_info{
             db_pool = DbPool,
             table_name = TableName,
             fields = [_|RestFields] = Fields,
             in_db_hook = InDbHook
            }, Record, F) ->
    [UndefId|RestVals] = Vals = record_to_vals(InDbHook, Record),
    {FilterUndefIdFields, FilterUndefIdVals} = 
        if
            UndefId =:= undefined ->
                {RestFields, RestVals};
            true ->
                {Fields, Vals}
        end,    
    SQL = iolist_to_binary(erl_mysql:F(TableName, {FilterUndefIdFields, [FilterUndefIdVals]})),
    case mysql_pool:query(DbPool, SQL) of
        {ok, [#mysql_ok{
                 insert_id = InsertId
                }]} ->
            if 
                UndefId =:= undefined ->
                    {ok, setelement(2, Record, InsertId)};
                true ->
                    {ok, Record}
            end;
        {ok, [Result]} ->
            ?DEBUG("db error ~p~n", [Result]),
            {error, Result};
        Reason ->
            ?DEBUG("db exit Reason ~p~n", [Reason]),
            {error, Reason}
    end.


%% 多行插入的混合接口的返回值实在不好做，干脆要么有id要么没有id，
%% 在表设计的时候已经决定了这点，正常设计不会出现有时候要向MySQL要id，有时候又不要。
%% 多行插入也就日志系统使用会有一定的优化，其他场合应该都一条一条插入
r_list_insert_withnot_id(#record_mysql_info{
                            db_pool = DbPool,
                            table_name = TableName,
                            fields = [_|RestFields],
                            in_db_hook = InDbHook
                           }, RecordList) 
  when is_list(RecordList) ->    
    FilterUndefIdValsList = [tl(record_to_vals(InDbHook, Record)) || Record <- RecordList],
    SQL = iolist_to_binary(erl_mysql:insert(TableName, {RestFields, FilterUndefIdValsList})),    
    run_affected(DbPool, SQL).

r_list_insert_with_id(#record_mysql_info{
                         db_pool = DbPool,
                         table_name = TableName,
                         fields = Fields,
                         in_db_hook = InDbHook
                        }, RecordList) 
  when is_list(RecordList) ->
    ValsList = [record_to_vals(InDbHook, Record) || Record <- RecordList],
    SQL = iolist_to_binary(erl_mysql:insert(TableName, {Fields, ValsList})),    
    run_affected(DbPool, SQL).

r_list_replace(#record_mysql_info{
                  db_pool = DbPool,
                  table_name = TableName,
                  fields = Fields,
                  in_db_hook = InDbHook
                 }, RecordList) 
  when is_list(RecordList) ->
    ValsList = [record_to_vals(InDbHook, Record) || Record <- RecordList],
    SQL = iolist_to_binary(erl_mysql:replace(TableName, {Fields, ValsList})),    
    run_affected(DbPool, SQL).  
    
%% update list的返回值不好定，所以不支持
r_update(#record_mysql_info{
            db_pool = DbPool,
            table_name = TableName,
            fields = [IdField|RestFields],
            in_db_hook = InDbHook
           }, Record) ->
    [Id|RestVals] = record_to_vals(InDbHook, Record),
    WhereClause = {IdField, '=', Id},
    UpdateClause = lists:zip(RestFields, RestVals),
    SQL = iolist_to_binary(erl_mysql:update(TableName, UpdateClause, WhereClause)),
    run_affected(DbPool, SQL).


r_delete(#record_mysql_info{
            db_pool = DbPool,
            table_name = TableName,
            fields = [IdField|_]
           }, Record) 
  when is_tuple(Record) ->
    Id = element(2, Record),
    WhereClause = {IdField, '=', Id},
    SQL = iolist_to_binary(erl_mysql:delete(TableName, WhereClause)),
    run_affected(DbPool, SQL);
r_delete(#record_mysql_info{
            db_pool = DbPool,
            table_name = TableName,
            fields = [IdField|_]
           }, RecordList) 
  when is_list(RecordList) ->
    Ids = [element(2, Record) || Record <- RecordList],
    WhereClause = {IdField, 'in', Ids},
    SQL = iolist_to_binary(erl_mysql:delete(TableName, WhereClause)),
    run_affected(DbPool, SQL).


%% -------------------- emysql封装 --------------------
run_affected(DbPool, SQL) ->
    case mysql_pool:query(DbPool, SQL) of
        {ok, [#mysql_ok{
                 affected_rows = AffectedRows
                }]}->
            {ok, AffectedRows};
        {ok, Result} ->
            ?DEBUG("db error Reason ~p~n", [Result]),
            {error, Result};
        Reason ->
            ?DEBUG("db exit Reason ~p~n", [Reason]),
            {error, Reason}
    end.

db_run_rows(#record_mysql_info{
               db_pool = DbPool,
               record_name = RecordName,
               out_db_hook = OutDbHook,
               fields = Fields
              }, SQL) ->
    run_rows(DbPool, SQL,
             fun(Result) ->
                     as_record(Result, RecordName, Fields, OutDbHook)
             end).

run_rows(DbPool, SQL) ->
    case mysql_pool:query(DbPool, SQL) of
        {ok, [#mysql_resultset{
                }=Result]} ->
            {ok, Result};
        {ok, [Result]}->
            ?DEBUG("db error Reason ~p~n", [Result]),
            {error, Result};
        Reason ->
            ?DEBUG("db exit Reason ~p~n", [Reason]),
            {error, Reason}
    end.

run_rows(DbPool, SQL, ResultFun) ->
    case run_rows(DbPool, SQL) of
        {ok, Result} ->
            {ok, ResultFun(Result)};
        Other ->
            Other
    end.

%% in_db_hook 存入数据库之前的操作
record_to_vals(InDbHook, Record) 
  when is_tuple(Record)->
    tl(tuple_to_list(db_hook(InDbHook, Record)));
record_to_vals(InDbHook, RecordList) 
  when is_list(RecordList)->
    [tl(tuple_to_list(db_hook(InDbHook, Record))) || Record <- RecordList].


db_hook(undefined, Record) ->
    Record;
db_hook(Fun, Record) ->
    Fun(Record).

as_record(#mysql_resultset{
             cols = Columns,
             rows = Rows
            }, 
          RecordName, Fields, Fun) ->
    S = lists:seq(1, length(Columns)),
    P = lists:zip([ binary_to_atom(Col, utf8) || Col <- Columns ], S),
    F = fun(FieldName) ->
                case proplists:lookup(FieldName, P) of
                    none ->
                        fun(_) -> undefined end;
                    {FieldName, Pos} ->
                        fun(Row) -> element(Pos, Row) end
                end
        end,
    Fs = [ F(FieldName) || FieldName <- Fields ],
    F1 = fun(Row0) ->
                 Row = list_to_tuple(Row0),
                 RecordData = [ Fx(Row) || Fx <- Fs ],
                 Record = list_to_tuple([RecordName|RecordData]),
                 if
                     Fun =:= undefined ->
                         Record;
                     true ->
                         Fun(Record)
                 end
         end,
    [ F1(Row) || Row <- Rows ].

as_record(Result, RecordName, Fields) ->
    as_record(Result, RecordName, Fields, fun(A) -> A end).

