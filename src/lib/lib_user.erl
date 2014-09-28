-module(lib_user).

-export([user_name/1]).

-include("db_user.hrl").
-include("define_time.hrl").
-include("common.hrl").

user(Id) ->
    ets_cache:get_with_default(user_cache, Id,
                               fun() ->
                                       case db_user:user(Id) of
                                           {ok, [User]} ->
                                               {expiration, User, ?FOUR_HOUR_SECONDS};
                                           {ok, []} ->
                                               {expiration, [], ?FOUR_HOUR_SECONDS};
                                           {error, Error} ->
                                               ?WARNING_MSG("db error ~p~n", [Error]),
                                               []
                                       end
                               end).

user_name(Id) ->
    case user(Id) of
        [] ->
            <<"未知用户"/utf8>>;
        #user{
           name = Name
          } ->
            Name
    end.
