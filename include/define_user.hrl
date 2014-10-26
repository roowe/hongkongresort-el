-ifndef(DEFINE_USER_HRL).
-define(DEFINE_USER_HRL, true).

-include("db_user.hrl").
-include("define_info_1.hrl").

-define(USER_GROUP_VISITOR, 0).
-define(USER_GROUP_USER, 1).
-define(USER_GROUP_MANAGER, 2).
-define(USER_GROUP_ADMIN, 3).

-endif.
