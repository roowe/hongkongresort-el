-module(mysql_misc).
-export([offset/2]).
-export([orientation/1]).


offset(Page, Num) ->
    (Page - 1) * Num.

%% Orientation 在request中的取值範圍是{+1, -1}, 分別表示ascending,  descending.
orientation(1) ->
    'asc';
orientation(-1) ->
    'desc'.
