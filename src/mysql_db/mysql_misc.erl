-module(mysql_misc).
-export([init/0]).
-export([offset/2]).

init() ->
    DefaultOptions = [{user, app_misc:get_env(default_mysql_user)},
                      {password, app_misc:get_env(default_mysql_password)},
                      {port, app_misc:get_env(default_mysql_port)},
                      {host, app_misc:get_env(default_mysql_host)},
                      {pool_size, app_misc:get_env(default_mysql_pool_size)}],
    [init_db(mochilists:set_defaults(DefaultOptions, options_hook(Conf))) || 
        Conf <- app_misc:get_env(mysql_conf)].

options_hook({PoolId, Options}) -> 
    [{pool_id, PoolId}|Options].

init_db(Options) ->
    Host = proplists:get_value(host, Options),
    Port = proplists:get_value(port, Options),
    User = proplists:get_value(user, Options),
    Password = proplists:get_value(password, Options),
    DataBase = proplists:get_value(db, Options),
    PoolId = proplists:get_value(pool_id, Options),
    Poolsize = proplists:get_value(pool_size, Options),
    emysql:add_pool(PoolId, Poolsize, User, Password, Host, Port, DataBase, utf8).

offset(Page, Num) ->
    (Page - 1) * Num.
