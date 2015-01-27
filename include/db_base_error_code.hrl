-ifndef(DB_BASE_ERROR_CODE_HRL).
-define(DB_BASE_ERROR_CODE_HRL, true).

-record(base_error_code, {
          error_code,                           %% 
          error_define = <<""/utf8>>,           %% 服务器宏定义
          error_desc                            %% 错误描述
         }).

-endif.
