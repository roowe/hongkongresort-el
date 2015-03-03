%% coding: utf-8
%% Warning:本文件由data_generate自动生成，请不要手动修改
-module(data_base_error_code).

-export([get/1]).

-include("common.hrl").

-include("db_base_error_code.hrl").

get(0) ->
    #base_error_code{error_code = 0,
		     error_define = <<"INFO_OK"/utf8>>,
		     error_desc = <<"成功"/utf8>>};
get(1) ->
    #base_error_code{error_code = 1,
		     error_define = <<"INFO_PARAMETER_MISS"/utf8>>,
		     error_desc = <<"参数不齐"/utf8>>};
get(2) ->
    #base_error_code{error_code = 2,
		     error_define = <<"INFO_ACTION_MISS"/utf8>>,
		     error_desc = <<"action没有实现"/utf8>>};
get(3) ->
    #base_error_code{error_code = 3,
		     error_define = <<"INFO_PARAMETER_ERROR"/utf8>>,
		     error_desc = <<"参数错误"/utf8>>};
get(4) ->
    #base_error_code{error_code = 4,
		     error_define = <<"INFO_BODY_NOT_EXIST"/utf8>>,
		     error_desc = <<"body为空"/utf8>>};
get(5) ->
    #base_error_code{error_code = 5,
		     error_define = <<"INFO_DB_ERROR"/utf8>>,
		     error_desc = <<"数据库操作失败"/utf8>>};
get(1001) ->
    #base_error_code{error_code = 1001,
		     error_define = <<"INFO_NOT_LOGGED_IN"/utf8>>,
		     error_desc = <<"用户未登陆"/utf8>>};
get(1002) ->
    #base_error_code{error_code = 1002,
		     error_define = <<"INFO_NOT_ADMIN"/utf8>>,
		     error_desc = <<"你不是管理员用户"/utf8>>};
get(1003) ->
    #base_error_code{error_code = 1003,
		     error_define = <<"INFO_USER_NOT_FOUND"/utf8>>,
		     error_desc = <<"用户没有找到"/utf8>>};
get(1004) ->
    #base_error_code{error_code = 1004,
		     error_define = <<"INFO_PSW_ERR"/utf8>>,
		     error_desc = <<"密碼錯誤"/utf8>>};
get(1005) ->
    #base_error_code{error_code = 1005,
		     error_define = <<"INFO_VISITOR"/utf8>>,
		     error_desc = <<"游客无法进行此操作"/utf8>>};
get(2001) ->
    #base_error_code{error_code = 2001,
		     error_define =
			 <<"INFO_CANNOT_COMMENT_ACTIVITY_HAS_BEGUN"/utf8>>,
		     error_desc = <<"活动已经开始，无法评论"/utf8>>};
get(2002) ->
    #base_error_code{error_code = 2002,
		     error_define = <<"INFO_COMMENT_NOT_FOUND"/utf8>>,
		     error_desc = <<"评论找不到"/utf8>>};
get(2003) ->
    #base_error_code{error_code = 2003,
		     error_define = <<"INFO_COMMENT_CONTENT_INVALID"/utf8>>,
		     error_desc = <<"评论内容不符合规定"/utf8>>};
get(3001) ->
    #base_error_code{error_code = 3001,
		     error_define = <<"INFO_ACTIVITY_NOT_FOUND"/utf8>>,
		     error_desc = <<"活动不存在"/utf8>>};
get(3002) ->
    #base_error_code{error_code = 3002,
		     error_define = <<"INFO_ACTIVITY_SAME_STATUS"/utf8>>,
		     error_desc = <<"活动状态一样，无须更改"/utf8>>};
get(3003) ->
    #base_error_code{error_code = 3003,
		     error_define =
			 <<"INFO_ACTIVITY_APPLICATION_DEADLINE_EXPIRED"/utf8>>,
		     error_desc = <<"活动已过期"/utf8>>};
get(3004) ->
    #base_error_code{error_code = 3004,
		     error_define = <<"INFO_ACTIVITY_NOT_ACCEPTED"/utf8>>,
		     error_desc = <<"活动未通过审核"/utf8>>};
get(3005) ->
    #base_error_code{error_code = 3005,
		     error_define = <<"INFO_ACTIVITY_JOINED"/utf8>>,
		     error_desc = <<"已经参加该活动"/utf8>>};
get(3006) ->
    #base_error_code{error_code = 3006,
		     error_define = <<"INFO_ACTIVITY_NOT_HOST"/utf8>>,
		     error_desc = <<"该活动不是你发起，无法选人"/utf8>>};
get(3007) ->
    #base_error_code{error_code = 3007,
		     error_define = <<"INFO_ACTIVITY_HAS_BEGUN"/utf8>>,
		     error_desc = <<"活动已开始"/utf8>>};
get(3008) ->
    #base_error_code{error_code = 3008,
		     error_define = <<"INFO_ACTIVITY_APPLIED_LIMIT"/utf8>>,
		     error_desc = <<"活动已经满人"/utf8>>};
get(3009) ->
    #base_error_code{error_code = 3009,
		     error_define = <<"INFO_ACTIVITY_SELECTED_LIMIT"/utf8>>,
		     error_desc = <<"选择的人数过多，请减少"/utf8>>};
get(Var1) ->
    ?WARNING_MSG("get not find ~p", [{Var1}]), [].
