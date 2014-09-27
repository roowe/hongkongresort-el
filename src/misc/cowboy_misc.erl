-module(cowboy_misc).

-export([not_qs_empty/1]).
-export([any_qs/1]).

not_qs_empty(List) ->
    lists:all(fun (<<>>) ->
                      false;
                  (undefined)->
                      false;
                  (_) ->
                      true
              end, List).

any_qs(List) ->
    lists:any(fun (<<>>) ->
                      false;
                  (undefined)->
                      false;
                  (_) ->
                      true
              end, List).
