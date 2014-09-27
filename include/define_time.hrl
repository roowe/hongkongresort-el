
-ifndef(DEFINE_TIME_HRL).
-define(DEFINE_TIME_HRL, true).

%% @doc 时间相关定义

-define(TIME_ZONE_SECONDS,      28800).                 %% 时区秒差
-define(TIME_ZONE_MILLISECONDS, 28800000).              %% 时区毫秒差

-define(DIFF_SECONDS_1970_1900, 2208988800).            %% 从1970年到现在的秒数
-define(DIFF_SECONDS_0000_1900, 62167219200).           %% 从0000到1970年的秒数
-define(HALF_MINITE_SECONDS,    30).                    %% 半分钟的秒数
-define(ONE_MINITE_SECONDS,     60).                    %% 1分钟的秒数
-define(TWO_MINITE_SECONDS,     120).                   %% 2分钟的秒数
-define(TWO_MINITE_MILLISECOND, 120000).              %% 2分钟的毫秒数
-define(THREE_MINITE_SECONDS,   180).                   %% 3分钟的秒数
-define(FIVE_MINITE_SECONDS,    300).                   %% 5分钟的秒数
-define(SIX_MINITE_SECONDS,     360).                   %% 6分钟的秒数
-define(TEN_MINITE_SECONDS,     600).                   %% 10分钟的秒数
-define(ELEVEN_MINITE_SECONDS,  660).                   %% 11分钟的秒数
-define(HALF_HOUR_SECONDS,      1800).                  %% 半小时的秒数
-define(ONE_HOUR_SECONDS,       3600).                  %% 一小时的秒数
-define(TWO_HOUR_SECONDS,       7200).                  %% 两小时的秒数
-define(FOUR_HOUR_SECONDS,      14400).                 %% 四小时的秒数
-define(SIX_HOUR_SECONDS,       21600).                 %% 六小时的秒数
-define(EIGHT_HOUR_SECONDS,     28800).                 %% 八小时的秒数
-define(TEN_HOUR_SECONDS,       36000).                 %% 十小时的秒数
-define(HALF_DAY_SECONDS,       43200).                 %% 十二小时的秒数
-define(ONE_DAY_SECONDS,        86400).                 %% 一天的时间（秒）
-define(ONE_DAY_MILLISECONDS,   86400000).              %% 一天时间（毫秒）
-define(ONE_WEEK_SECONDS,       604800).                %% 一周的时间（秒）
-define(TWO_WEEK_SECONDS,       1209600).               %% 两周的时间（秒）
-define(ONE_WEEK_MILLISECONDS,  604800000).             %% 一周时间（毫秒）
-define(ONE_YEAR_SECONDS,       31536000).              %% 一年的时间（秒）
-define(DIFF_SECONDS_0000_1970, 62167219200).           %% 0000年到1970年的秒数

-define(MONDAY,        1).
-define(TUESDAY,       2).
-define(WEDNESDAY,     3).
-define(THURSDAY,      4).
-define(FRIDAY,        5).
-define(SATURDAY,      6).
-define(SUNDAY,        7).


-endif.

