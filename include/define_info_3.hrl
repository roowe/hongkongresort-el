%% version : 6188b028fccfec675c42c6f1c739fb5f
%% 自动生成，请勿手动修改，避免重新生成的时候，造成不必要的数据丢失
-ifndef(DEFINE_INFO_3_HRL).
-define(DEFINE_INFO_3_HRL, true).

-define(INFO_ACTIVITY_NOT_FOUND, 3001). %% 活动不存在
-define(INFO_ACTIVITY_SAME_STATUS, 3002). %% 活动状态一样，无须更改
-define(INFO_ACTIVITY_APPLICATION_DEADLINE_EXPIRED, 3003). %% 活动已过期
-define(INFO_ACTIVITY_NOT_ACCEPTED, 3004). %% 活动未通过审核
-define(INFO_ACTIVITY_JOINED, 3005). %% 已经参加该活动
-define(INFO_ACTIVITY_NOT_HOST, 3006). %% 该活动不是你发起，无法选人
-define(INFO_ACTIVITY_HAS_BEGUN, 3007). %% 活动已开始
-define(INFO_ACTIVITY_APPLIED_LIMIT, 3008). %% 活动已经满人
-define(INFO_ACTIVITY_SELECTED_LIMIT, 3009). %% 选择的人数过多，请减少
-endif.
