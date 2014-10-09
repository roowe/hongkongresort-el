-module(comment_controller).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-export([execute_get/3]).
-export([pack/2]).

-include("common.hrl").
-include("db_comment.hrl").
-include("define_info_0.hrl").
%% 使用宏的原因是因为Emacs解析<<>>和{}容易混了，所以用宏，同时，日后也方便修改
-define(ACTION_QUERY, [<<"query">>]).
-define(ACTION_SUB_QUERY, [<<"sub">>, <<"query">>]).

-define(PARAM_PAGE, <<"page">>).
-define(PARAM_NUM_ITEMS, <<"num_items">>).
-define(PARAM_ACTIVITY_ID, <<"activity_id">>).
-define(PARAM_PARENT_ID, <<"parent_id">>).

init(_Type, Req, _Env) ->
	{ok, Req, undefined}.

handle(Req, State) ->
    GetParameter = [{?ACTION_QUERY, [{?PARAM_PAGE, int, required},
                                     {?PARAM_NUM_ITEMS, int, required},
                                     {?PARAM_ACTIVITY_ID, int, required}]},
                    {?ACTION_SUB_QUERY, [{?PARAM_PAGE, int, required},
                                         {?PARAM_NUM_ITEMS, int, required},
                                         {?PARAM_PARENT_ID, int, required}]}],
    Req1 = controller_helper:execute(?MODULE, Req, [{get_parameter, GetParameter}]),
	{ok, Req1, State}.

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

pack(?ACTION_QUERY, {Page, HeadComments, SubComments}) ->
    ?DEBUG("~p ~p~n", [length(HeadComments), length(SubComments)]),
    ?JSON([{page, Page},
           {comments, to_client_data(HeadComments, SubComments, [[]])}]);
pack(?ACTION_SUB_QUERY, {Page, List}) ->
    ?JSON([{page, Page},
           {sub_comments, [to_sub_comment_kv(Comment) || Comment <- List]}]).
  
   
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

