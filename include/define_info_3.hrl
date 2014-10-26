-ifndef(DEFINE_INFO_3_HRL).
-define(DEFINE_INFO_3_HRL, true).
%% 活动相关错误
-define(INFO_ACTIVITY_NOT_FOUND, 3001).       %%  活动不存在
-define(INFO_ACTIVITY_SAME_STATUS, 3002).       %%  活动状态一样，无须更改
-define(INFO_ACTIVITY_APPLICATION_DEADLINE_EXPIRED, 3003).       %%  活动已过期

-endif.
