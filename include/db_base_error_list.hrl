%% Warning:本文件由make_record自动生成，请不要手动修改
-ifndef(DB_BASE_ERROR_LIST_HRL).
-define(DB_BASE_ERROR_LIST_HRL, true).
%% base_error_list => base_error_list
-record(base_error_list, {
          error_code,                           %% 
          error_define = <<""/utf8>>,           %% 服务器宏定义
          error_desc                            %% 错误描述
         }).
-endif.
