-ifndef(DEFINE_ACTIVITY_HRL).
-define(DEFINE_ACTIVITY_HRL, true).

-include("db_activity.hrl").
-include("define_info_3.hrl").

-define(ACTIVITY_STATUS_CREATED, 0).
-define(ACTIVITY_STATUS_PENDING, 1).
-define(ACTIVITY_STATUS_REJECTED, 2).
-define(ACTIVITY_STATUS_ACCEPTED, 3).
-define(ACTIVITY_STATUS_DELETED, 4).

-endif.
