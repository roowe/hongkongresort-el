-module(lib_user).

-export([user/1, user_name/1]).
-export([user_id_by_token/1]).
-export([is_admin_user/1]).

-export([incr_unread_count/1]).

-include("db_user.hrl").
-include("db_login.hrl").
-include("define_time.hrl").
-include("common.hrl").
-include("define_user.hrl").

user(Id) ->
    case db_user:user(Id) of
        {ok, [User]} ->
            User;
        {ok, []} ->
            []
    end.

user_name(Id) ->
    case user(Id) of
        [] ->
            <<"未知用户"/utf8>>;
        #user{
           name = Name
          } ->
            Name
    end.

user_id_by_token(undefined) ->
    {ok, undefined};
user_id_by_token(<<"">>) ->
    {ok, undefined};
user_id_by_token(Token) ->
    case db_login:login_info(Token) of
        {ok, [#login{
                 user_id = UserId
                }]} ->
            {ok, UserId};
        _ ->
            {fail, ?INFO_NOT_LOGIN}
    end.



is_admin_user(Token) ->
    case lib_user:user_id_by_token(Token) of
        ?FAIL_REASON ->
            ?FAIL_REASON;
        {ok, UserId} ->
            case lib_user:user(UserId) of
                [] ->
                    ?FAIL(?INFO_NOT_FIND);
                #user{
                   group_id = ?USER_GROUP_ADMIN
                  } ->
                    ok;
                _ ->
                    ?FAIL(?INFO_NOT_ADMIN)
            end
    end.

incr_unread_count(Id) ->
    ?DEBUG("incr unread_count ~p~n", [Id]),
    db_user:incr_unread_count(Id).
