-ifndef(DEFINE_INFO_3_HRL).
-define(DEFINE_INFO_3_HRL, true).
%% 活动相关错误
-define(INFO_ACTIVITY_NOT_FOUND, 3001).       %%  活动不存在
-define(INFO_ACTIVITY_SAME_STATUS, 3002).       %%  活动状态一样，无须更改
-define(INFO_ACTIVITY_APPLICATION_DEADLINE_EXPIRED, 3003).       %%  活动已过期
-define(INFO_ACTIVITY_STATUS_NOT_ACCEPTED, 3004).       %%  活动未通过审核
-define(INFO_ACTIVITY_JOINED, 3005).       %%  已经参加该活动

-endif.
