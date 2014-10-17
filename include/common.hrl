-ifndef(COMMON_HRL).
-define(COMMON_HRL, true).
%% 本头文件会被每个module包含，不是需要全局的定义，请不要放在这里，否则会引起频繁编译的问题
-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include("define_logger.hrl").
-include("define_http.hrl").
-include("define_ws_cmd.hrl").

-define(APP_NAME, hongkongresort). %% 应用没启动起来的时候会用到

-define(MAX_WAIT, 16#ffffffff).

-define(JSON(Value), {Value}).

-endif.
