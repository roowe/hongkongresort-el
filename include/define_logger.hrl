-ifndef(DEFINE_LOGGER_HRL).
-define(DEFINE_LOGGER_HRL, true).
%% ---------------------------------
%% Logging mechanism
%% Print in standard output

-define(TAG_PRINT(Tag, Format, Args),
        io:format("(~p:~s:~p:~p:~p) : " ++ Format, 
                  [Tag, hmisctime:time_format(now()), self(), ?MODULE, ?LINE] ++ Args)).
%% -define(PRINT(Format, Args),
%%         io:format("(~s:~p:~p:~p) : " ++ Format, 
%%                   [hmisctime:time_format(now()), self(), ?MODULE, ?LINE] ++ Args)).
-define(PRINT(Format, Args),
        io:format("(~p:~p:~p)" ++ Format, [self(), ?MODULE, ?LINE] ++ Args)).
-define(DEBUG, lager:debug).
-define(INFO_MSG, lager:info).
-define(NOTICE, lager:notice).
-define(WARNING_MSG, lager:warning).
-define(ERROR_MSG, lager:error).
-define(CRITICAL, lager:critical).
-define(ALERT, lager:alert).
-define(EMERGENCY, lager:emergency).
-endif.

