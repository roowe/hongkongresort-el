-module(mysql_pool).

-export([init/0]).

-export([query/2, transaction/2]).


init() ->
    DefaultOptions = [{user, app_misc:get_env(default_mysql_user)},
                      {password, app_misc:get_env(default_mysql_password)},
                      {port, app_misc:get_env(default_mysql_port)},
                      {host, app_misc:get_env(default_mysql_host)},
                      {pool_size, app_misc:get_env(default_mysql_pool_size)}],
    [hongkongresort_sup:start_poolboy(init_db(mochilists:set_defaults(DefaultOptions, options_hook(Conf)))) || 
        Conf <- app_misc:get_env(mysql_conf)].

options_hook({PoolId, Options}) -> 
    [{pool_id, PoolId}|Options].

init_db(Options) ->
    Host = proplists:get_value(host, Options),
    Port = proplists:get_value(port, Options),
    User = proplists:get_value(user, Options),
    Password = proplists:get_value(password, Options),
    DataBase = proplists:get_value(db, Options),
    PoolId = pool_name(proplists:get_value(pool_id, Options)),
    Poolsize = proplists:get_value(pool_size, Options),
    MySqlOptions = [{host, Host}, {port, Port}, 
                    {user, User}, {password, Password},
                    {database, DataBase}],
    PoolArgs = [{strategy, fifo}, {name, {local, PoolId}}, {worker_module, mysql}, {size, Poolsize}, {max_overflow, Poolsize+10}],
    poolboy:child_spec(PoolId, PoolArgs, MySqlOptions).

query(Pid, SQL) 
  when is_pid(Pid) ->
    mysql:query(Pid, SQL, 5000);
query(Name, SQL) ->
    poolboy:transaction(pool_name(Name), fun(Db) ->
                                                 mysql:query(Db, SQL, 5000)
                                         end).
transaction(Name, Fun) ->
    poolboy:transaction(pool_name(Name), fun(Db) ->
                                                 mysql:transaction(Db, Fun, [Db], infinity)
                                         end).

pool_name(hongkongresort) ->
    hongkongresort_db.
