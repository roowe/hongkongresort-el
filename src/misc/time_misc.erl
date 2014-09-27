-module(time_misc).
-export([
         unixtime/0
        ]).

-export([datetime_to_timestamp/2]).

-include("define_time.hrl").

current() -> 
    os:timestamp().

%% 取得当前的unix时间戳，秒级
unixtime() ->
    {M, S, _} = current(),
    M * 1000000 + S.

%% 由于时间{Hour, Min, Sec}本身具有时区，所以转化成时间戳的时候，要减掉东八区
datetime_to_timestamp({Year, Month, Day}, {Hour, Min, Sec}) ->
    OrealTime =  calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}}),
    ZeroSecs = calendar:datetime_to_gregorian_seconds({{Year, Month, Day}, {Hour, Min, Sec}}),
    ZeroSecs - OrealTime - ?TIME_ZONE_SECONDS.
