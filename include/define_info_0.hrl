-ifndef(DEFINE_INFO_0_HRL).
-define(DEFINE_INFO_0_HRL, true).
%% 系统错误
-define(INFO_OK,                                0).       %%  成功
-define(INFO_PARAMETER_MISS,                    1).       %%  参数不齐
-define(INFO_ACTION_MISS,                       2).       %%  action没有实现
-define(INFO_PARAMETER_ERROR,                   3).       %%  参数错误
-define(INFO_BODY_NOT_EXIST,                    4).       %%  body为空
-define(INFO_DB_ERROR,                    5).       %%  数据库操作失败

-endif.
