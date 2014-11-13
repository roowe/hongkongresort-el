-module(comment_controller).

-export([init/2]).

-export([terminate/3]).

-export([execute_get/3, execute_post/3]).
-export([get_pack/2]).

-include("common.hrl").
-include("db_comment.hrl").
-include("db_activity.hrl").
-include("db_notification.hrl").

-include("define_info_0.hrl").
-include("define_info_2.hrl").
%% 使用宏的原因是因为Emacs解析<<>>和{}容易混了，所以用宏，同时，日后也方便修改
-define(ACTION_QUERY, [<<"query">>]).
-define(ACTION_SUB_QUERY, [<<"sub">>, <<"query">>]).

-define(POST_ACTION_SUBMIT, [<<"submit">>]).
-define(POST_ACTION_SUB_SUBMIT, [<<"sub">>, <<"submit">>]).

-define(PARAM_PAGE, <<"page">>).
-define(PARAM_NUM_ITEMS, <<"num_items">>).
-define(PARAM_ACTIVITY_ID, <<"activity_id">>).
-define(PARAM_PARENT_ID, <<"parent_id">>).
-define(PARAM_TOKEN, <<"token">>). 
-define(PARAM_CONTENT, <<"content">>).
-define(PARAM_TO, <<"to">>).
-define(PARAM_PREDECESSOR_ID, <<"predecessor_id">>).


init(Req, _Env) ->
    GetParameter = [{?ACTION_QUERY, [{?PARAM_PAGE, int, required},
                                     {?PARAM_NUM_ITEMS, int, required},
                                     {?PARAM_ACTIVITY_ID, int, required}]},
                    {?ACTION_SUB_QUERY, [{?PARAM_PAGE, int, required},
                                         {?PARAM_NUM_ITEMS, int, required},
                                         {?PARAM_PARENT_ID, int, required}]}],
    PostParameter = [{?POST_ACTION_SUBMIT, [{?PARAM_TOKEN, binary, required},
                                            {?PARAM_ACTIVITY_ID, int, required},
                                             {?PARAM_CONTENT, binary, required}]},
                     {?POST_ACTION_SUB_SUBMIT, [{?PARAM_TOKEN, binary, required},
                                                {?PARAM_ACTIVITY_ID, int, required},
                                                {?PARAM_CONTENT, binary, required},
                                                {?PARAM_TO, int, required},
                                                {?PARAM_PREDECESSOR_ID, int, required},
                                                {?PARAM_PARENT_ID, int, required}]}],
    ControllerOpts = [{get_parameter, GetParameter},
                      {post_parameter, PostParameter}],
	{controller_helper, Req, ControllerOpts}.

%%  /el/comment/query?page=1&num_items=10&activity_id=30
execute_get(?ACTION_QUERY, [Page, NumItems, ActivityId], _Req) ->
    {ok, HeadComments} = db_comment:head_comments_page(ActivityId, Page, NumItems),
    HeadCommentIds = [Id || #comment{
                               id = Id
                              } <- HeadComments],
    {ok, SubComments} = db_comment:sub_comments(HeadCommentIds),
    {pack, {Page, HeadComments, SubComments}};
%% /el/comment/sub/query?page=1&num_items=10&parent_id=21
execute_get(?ACTION_SUB_QUERY, [Page, NumItems, ParentId], _Req) ->   
    {ok, List} = db_comment:sub_comments(Page, NumItems, ParentId),
    {pack, {Page, List}}.

get_pack(?ACTION_QUERY, {Page, HeadComments, SubComments}) ->
    ?DEBUG("~p ~p~n", [length(HeadComments), length(SubComments)]),
    ?JSON([{page, Page},
           {comments, to_client_data(HeadComments, SubComments, [[]])}]);
get_pack(?ACTION_SUB_QUERY, {Page, List}) ->
    ?JSON([{page, Page},
           {sub_comments, [to_sub_comment_kv(Comment) || Comment <- List]}]).

%% /el/comment/submit
%% token=0a029a1451b987fd3401f3820ec5139a&content=test嗯&activity_id=5
execute_post(?POST_ACTION_SUBMIT, [Token, ActivityId, Content], _Req) ->
    case check_comment_submit(ActivityId, Token) of
        {fail, Reason} ->
            {fail, Reason};
        {ok, UserId, #activity{
                        id = ActivityId,
                        host_id = HostId
                       } = Activity} ->
            case db_comment:insert(#comment{
                                      content = Content,
                                      activity_id = ActivityId,
                                      from = UserId,
                                      generated_time = time_misc:long_unixtime()
                                     }) of 
                {ok, #comment{
                        id = CommentId
                       }} ->
                    Notification = #notification{
                                      cmd = ?S2C_COMMENT_SUBMIT,
                                      activity_id = ActivityId,
                                      comment_id = CommentId,
                                      from = UserId,
                                      to = HostId,
                                      content = notice_cotent(Activity, UserId)
                                     },
                    lib_notification:insert_and_push(Notification,
                                                     fun notification_pack/1),
                    {fail, ?INFO_OK};
                {error, _} ->
                    {fail, ?INFO_DB_ERROR}
            end                
    end;
%% /el/comment/sub/submit
%% token=0b8e47d16124c77b1a406048967757e4&content=test嗯&activity_id=5&to=10&predecessor_id=47&parent_id=47
execute_post(?POST_ACTION_SUB_SUBMIT, [Token, ActivityId, Content, 
                                       To, PredecessorId, ParentId], _Req) ->
    case check_comment_submit(ActivityId, Token) of
        {fail, Reason} ->
            {fail, Reason};
        {ok, UserId, Activity} ->
            case check_sub_comment_submit(ActivityId, PredecessorId, ParentId, To) of
                {fail, Reason} ->
                    {fail, Reason};
                {ok, ParentComment} ->
                    Comment = #comment{
                                 content = Content,
                                 activity_id = ActivityId,
                                 from = UserId,
                                 to = To,
                                 predecessor_id = PredecessorId,
                                 parent_id = ParentId,
                                 generated_time = time_misc:long_unixtime()
                                },
                    case db_comment:insert(Comment) of
                        {ok, #comment{
                                id = CommentId
                               }} ->
                            Notification = #notification{
                                              cmd = ?S2C_COMMENT_SUB_SUBMIT,
                                              activity_id = ActivityId,
                                              comment_id = CommentId,
                                              from = UserId,
                                              to = To,
                                              content = notice_cotent(Activity, UserId)
                                             },
                            lib_notification:insert_and_push(Notification, 
                                                             fun notification_pack/1),
                            db_comment:update(ParentComment#comment{
                                                num_children = ParentComment#comment.num_children + 1
                                               }),
                            {fail, ?INFO_OK};
                        {error, _} ->                            
                            {fail, ?INFO_DB_ERROR}
                    end
            end            
    end.
     

check_comment_submit(ActivityId, Token) ->
    case db_activity:find(ActivityId) of
        ?FAIL_REASON ->
            ?FAIL_REASON;
        {ok, #activity{
                 begin_time = BeginTimeStamp
                } = Activity} ->
            Now = time_misc:long_unixtime(),
            if
                Now >= BeginTimeStamp ->
                    {fail, ?INFO_CANNT_COMMENT_ACTIVITY_HAS_BEGUN};
                true ->
                    case lib_user:user_id_by_token(Token) of
                        {fail, Reason} ->
                            {fail, Reason};
                        {ok, UserId} ->
                            {ok, UserId, Activity}
                    end
            end
    end.

check_sub_comment_submit(ActivityId, Same, Same, To) ->
    case db_comment:find(Same) of
        {ok, [#comment{
                 from = To,
                 parent_id = -1,
                 activity_id = ActivityId
                } = ParentComment]} ->
            {ok, ParentComment};
        _Other ->
            ?DEBUG("_Other ~p~n", [_Other]),
            {fail, ?INFO_COMMENT_NOT_FOUND}
    end;
check_sub_comment_submit(ActivityId, PredecessorId, ParentId, To) ->
    case db_comment:find([PredecessorId, ParentId]) of
        {ok, [#comment{
                 from = To,
                 parent_id = ParentId,
                 activity_id = ActivityId
                }, 
              #comment{
                 activity_id = ActivityId
                } = ParentComment]} ->
            {ok, ParentComment};
        _Other ->
            ?DEBUG("_Other ~p~n", [_Other]),
            {fail, ?INFO_COMMENT_NOT_FOUND}
    end.

to_client_data([], _, [_|Response]) ->
    Response;
%% 没有次级评论，剩下都是首级评论
to_client_data([HeadComment|RestHeadComments], 
               [], [AccSubComments|Response]) ->
    to_client_data(RestHeadComments, [], 
                   [[], to_head_comment_kv(HeadComment, AccSubComments)|Response]);
to_client_data([HeadComment|RestHeadComments] = HeadComments, 
               [SubComment|RestSubComments] = SubComments, [AccSubComments|Response]) ->
    if
        HeadComment#comment.id =:= SubComment#comment.parent_id ->
            to_client_data(HeadComments, RestSubComments, [[SubComment|AccSubComments]|Response]);
        true ->
            to_client_data(RestHeadComments, SubComments, 
                           [[], to_head_comment_kv(HeadComment, AccSubComments)|Response])            
    end.

-define(comment_kv(Field), {Field, Comment#comment.Field}).   
%% from是评论者，
%% to是被评论者
-define(READ_MORE_NO, 1).
-define(READ_MORE_YES, 2).

to_head_comment_kv(HeadComment, SubComments) ->
    SubCommentKVs = [to_sub_comment_kv(Comment) || Comment <- SubComments],
    ?JSON([{num_children, HeadComment#comment.num_children}, {sub_comments, SubCommentKVs}|to_comment_kv(HeadComment)]).

to_sub_comment_kv(Comment) ->
    ?JSON([?comment_kv(to),
           {to_name, lib_user:user_name(Comment#comment.to)}|to_comment_kv(Comment)]).

to_comment_kv(Comment) ->
    [?comment_kv(id),
     ?comment_kv(parent_id),
     ?comment_kv(from),
     {from_name, lib_user:user_name(Comment#comment.from)},
     ?comment_kv(generated_time),
     ?comment_kv(content)].

terminate(_Reason, _Req, _State) ->
	ok.

%% "你的活動 id <activity_id> 收到一條來到 <from> 的評論”.
notice_cotent(#activity{
                 id = ActivityId
                }, FromId) ->
    <<"你的活動 id "/utf8, 
      (erlang:integer_to_binary(ActivityId))/binary, 
      " 收到一條來到 "/utf8, 
      (erlang:integer_to_binary(FromId))/binary, 
      " 的評論"/utf8>>.

notification_pack(Notification) ->
    ?JSON([{id, Notification#notification.id},
           {activity_id, Notification#notification.activity_id},
           {from, Notification#notification.from},
           {content, Notification#notification.content}]).
