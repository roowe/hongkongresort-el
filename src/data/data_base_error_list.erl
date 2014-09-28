%% coding: utf-8
%% Warning:本文件由data_generate自动生成，请不要手动修改
-module(data_base_error_list).

-export([get/1]).

-include("common.hrl").

-include("db_base_error_list.hrl").

get(2) ->
    #base_error_list{error_code = 2,
		     error_define = <<"INFO_ACTION_MISS"/utf8>>,
		     error_desc = <<"没有该action"/utf8>>};
get(1) ->
    #base_error_list{error_code = 1,
		     error_define = <<"INFO_PARAMETER_MISS"/utf8>>,
		     error_desc = <<"参数不齐"/utf8>>};
get(0) ->
    #base_error_list{error_code = 0,
		     error_define = <<"INFO_OK"/utf8>>,
		     error_desc = <<"成功"/utf8>>};
get(Var1) ->
    ?WARNING_MSG("get not find ~p", [{Var1}]), [].