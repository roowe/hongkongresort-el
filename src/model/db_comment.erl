-module(db_comment).

-export([head_comments_page/3]).
-export([sub_comments/1]).
-export([sub_comments/3]).

-export([fix_comment/0]).

-include("db_comment.hrl").
-include("define_mysql.hrl").

-define(TABLE_CONF, #record_mysql_info{
                       db_pool = hongkongresort,
                       table_name = comment,
                       record_name = comment,
                       fields = record_info(fields, comment),
                       out_db_hook = fun(#comment{
                                            generated_time = GeneratedTime
                                           } = Comment) ->
                                             Comment#comment{
                                               generated_time = time_misc:db_timestamp_format(GeneratedTime)
                                              }
                                     end
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
-define(HEAD_COMMENT_PARENT_ID, -1).

head_comments_page(ActivityId, Page, Num) ->
    Offset = mysql_misc:offset(Page, Num),
    db_mysql_base:select(?TABLE_CONF, {{activity_id, '=', ActivityId},
                                       'and',
                                       {parent_id, '=', ?HEAD_COMMENT_PARENT_ID}}, 
                         [{limit, Offset, Num}]).

sub_comments(ParentIdS) ->
    SQLs = [[<<"(">>, erl_mysql:select('*', comment, {parent_id, '=', Id}, {limit, 4}), <<")">>] || Id <- ParentIdS],
    SQL = iolist_to_binary(string:join(SQLs, " UNION ")),
    db_mysql_base:db_run_rows(?TABLE_CONF, SQL).

sub_comments(Page, Num, ParentId) ->
    Offset = mysql_misc:offset(Page, Num),
    db_mysql_base:select(?TABLE_CONF, {parent_id, '=', ParentId}, 
                         [{limit, Offset, Num}]).

fix_comment() ->
    {ok, Comments} = db_mysql_base:select(?TABLE_CONF, undefined),
    FixFun = fun(#comment{
                    predecessor_id = PredecessorId
                   }=Comment) ->
                     case lists:keyfind(PredecessorId, #comment.id, Comments) of
                         false ->
                             ignore;
                         #comment{
                            to = CommenterId
                           } ->
                             update(Comment#comment{
                                      from = CommenterId
                                     })
                     end
             end,
    [FixFun(Comment) || Comment <- Comments].
