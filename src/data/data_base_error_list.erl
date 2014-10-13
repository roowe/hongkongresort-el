%% coding: utf-8
%% Warning:本文件由data_generate自动生成，请不要手动修改
-module(data_base_error_list).

-export([get/1]).

-include("common.hrl").

-include("db_base_error_list.hrl").

get(2) ->
    #base_error_list{error_code = 2,
		     error_define = <<"INFO_ACTION_MISS"/utf8>>,
		     error_desc = <<"action没有实现"/utf8>>};
get(1) ->
    #base_error_list{error_code = 1,
		     error_define = <<"INFO_PARAMETER_MISS"/utf8>>,
		     error_desc = <<"参数不齐"/utf8>>};
get(0) ->
    #base_error_list{error_code = 0,
		     error_define = <<"INFO_OK"/utf8>>,
		     error_desc = <<"成功"/utf8>>};
get(1001) ->
    #base_error_list{error_code = 1001,
		     error_define = <<"INFO_NOT_LOGIN"/utf8>>,
		     error_desc = <<"用户未登陆"/utf8>>};
get(3) ->
    #base_error_list{error_code = 3,
		     error_define = <<"INFO_PARAMETER_ERROR"/utf8>>,
		     error_desc = <<"参数错误"/utf8>>};
get(2002) ->
    #base_error_list{error_code = 2002,
		     error_define =
			 <<"INFO_CANNT_COMMENT_ACTIVITY_NOT_FOUND"/utf8>>,
		     error_desc = <<"活动不存在，无法评论"/utf8>>};
get(2001) ->
    #base_error_list{error_code = 2001,
		     error_define =
			 <<"INFO_CANNT_COMMENT_ACTIVITY_HAS_BEGUN"/utf8>>,
		     error_desc = <<"活动已经开始，无法评论"/utf8>>};
get(4) ->
    #base_error_list{error_code = 4,
		     error_define = <<"INFO_BODY_NOT_EXIST"/utf8>>,
		     error_desc = <<"body为空"/utf8>>};
get(2003) ->
    #base_error_list{error_code = 2003,
		     error_define = <<"INFO_COMMENT_NOT_FOUND"/utf8>>,
		     error_desc = <<"评论找不到"/utf8>>};
get(5) ->
    #base_error_list{error_code = 5,
		     error_define = <<"INFO_DB_ERROR"/utf8>>,
		     error_desc = <<"数据库操作失败"/utf8>>};
get(Var1) ->
    ?WARNING_MSG("get not find ~p", [{Var1}]), [].
