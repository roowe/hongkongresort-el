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

-endif.
