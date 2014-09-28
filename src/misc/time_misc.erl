-module(time_misc).
-export([
         unixtime/0
        ]).

-export([datetime_to_timestamp/2]).

-export([db_timestamp_format/1]).

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

%% 2014-09-09 16:14:43	
db_timestamp_format({datetime, {{Year, Month, Day}, {Hour, Minute, Second}}}) ->
    <<(integer_to_binary(Year))/binary, "-", 
      (one_to_two(Month))/binary, "-", 
      (one_to_two(Day))/binary, " ",
      (one_to_two(Hour))/binary, ":", 
      (one_to_two(Minute))/binary,  ":", 
      (one_to_two(Second))/binary>>.

one_to_two(One) 
  when One < 10 ->
    <<"0", (integer_to_binary(One))/binary>>;
one_to_two(Two) ->
    integer_to_binary(Two).
