-ifndef(DEFINE_WS_CMD_HRL).
-define(DEFINE_WS_CMD_HRL, true).

%% activity: (1 << 31),
%% user: (1 << 30),
%% comment: (1 << 29),
%% assessment: (1 << 28),
%% notification: (1 << 27)

-define(S2C, (1 bsl 10)).
-define(C2S, 0).

-define(COMMENT_RES_ID, (1 bsl 29)).
-define(S2C_COMMENT_RES_ID, ?COMMENT_RES_ID+?S2C).

-define(S2C_COMMENT_SUBMIT, ?S2C_COMMENT_RES_ID + 0).
-define(S2C_COMMENT_SUB_SUBMIT, ?S2C_COMMENT_RES_ID + 1).

-define(ACTIVITY_RES_ID, (1 bsl 31)).
-define(S2C_ACTIVITY_RES_ID, ?ACTIVITY_RES_ID+?S2C).
-define(S2C_ACTIVITY_ACCEPTED, ?S2C_ACTIVITY_RES_ID+0).
-define(S2C_ACTIVITY_REJECTED, ?S2C_ACTIVITY_RES_ID+1).
-define(S2C_ACTIVITY_DELETED, ?S2C_ACTIVITY_RES_ID+2).
-define(S2C_ACTIVITY_BEGIN, ?S2C_ACTIVITY_RES_ID+3).

-define(USER_RES_ID, (1 bsl 30)).
-define(S2C_USER_RES_ID, ?USER_RES_ID+?S2C).
-define(S2C_PARTICIPANTS_UPDATE, ?S2C_USER_RES_ID + 0).
-define(S2C_ACTIVITY_JOIN, ?S2C_USER_RES_ID + 1).

-endif.
