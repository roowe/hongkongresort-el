%% 本模块主要功能就是启动的时候将lager的log相对路径是基于项目的根目录的，最后并将不同节点的log扔到不同目录
%% 大概就像/home/roowe/happytree/server_p04/logs/p04_db_disc_1@127.0.0.1
%% 其次，不同级别的log文件是不需要配置的，（配置也没用，会被强制覆盖掉）是用级别推算出来相应文件名，看log_file_name函数
-module(lager_misc).

-export([start/0]).

-include("common.hrl").

start() ->
    ok = application:load(lager),
    {ok, Handlers} = application:get_env(lager, handlers),
    application:set_env(lager, handlers, may_add_root_dir(Handlers)),
    application:set_env(lager, crash_log, crash_log()),
    lager:start().

may_add_root_dir(Handlers) ->
    %% -detached
    %% Starts the Erlang runtime system detached from the system console. 
    %% Useful for running daemons and backgrounds processes. Implies -noinput.
    %% daemons的时候移除lager_console_backend
    LogDir = log_dir(),
    lists:foldl(fun ({lager_file_backend, FileConfig}, Acc) ->
                        Level = proplists:get_value(level, FileConfig),
                        LogFileName = log_file_name(Level),
                        [{lager_file_backend, 
                          lists:keystore(
                            file, 1, FileConfig,
                            {file, filename:join(LogDir, LogFileName)})
                         } | Acc];
                    ({lager_console_backend, _}=ConsoleConfig, Acc) ->
                        case init:get_argument(noinput) of
                            error ->
                                [ConsoleConfig|Acc];
                            _ ->
                                Acc
                        end;
                    (Other, Acc) ->
                        [Other|Acc]
                end, [], Handlers).

log_file_name(Level) ->
    lists:concat([?APP_NAME, "-", Level, ".log"]).

log_dir() ->
    LogDir = app_misc:get_env(log_dir, "logs/"),    
    case LogDir of
        "/" ++ _ ->
            filename:join([LogDir, node()]);
        _ ->
            filename:join([app_misc:root_dir(), LogDir, node()])
    end.
   

crash_log() ->
    filename:join(log_dir(), log_file_name(crash)).
