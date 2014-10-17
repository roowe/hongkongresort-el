-module(log_misc).

-export([start_lager/1, set_loglevel/1]).

start_lager(Level) ->
    ok = application:load(lager),    
    LagerBackends0 = [lager_file_backend(Level)],
    LagerBackends = lager_console_backend(Level, LagerBackends0),
    application:set_env(lager, handlers, LagerBackends),
    application:set_env(lager, crash_log, crash_log()),
    application:set_env(lager, crash_log_count, 30),
    lager:start().    

lager_file_backend(Level) ->
    {lager_file_backend, 
     [
      {level, Level}, 
      {date, "$W0D23"},
      {count, 20}, %% 默认每个文件10M
      {file, lager_file_backend_file()}
     ]}.

is_console() ->
    init:get_argument(noinput) =:= error.

lager_console_backend(Level, LagerBackends) ->
    %% -detached
    %% Starts the Erlang runtime system detached from the system console. 
    %% Useful for running daemons and backgrounds processes. Implies -noinput.
    %% daemons的时候移除lager_console_backend    
    case is_console() of
        true ->
            %% 交互式
            LagerConsoleBackend = lager_console_backend(Level),                
            [LagerConsoleBackend|LagerBackends];
        _ ->
            LagerBackends
    end.

lager_console_backend(Level) ->   
    {lager_console_backend, %% config options [Level,{Formatter,FormatterConfig}]
     [Level, {lager_default_formatter, 
              %% The properties pid, file, line, module, function, and node will always exist if the parser transform is used.
              %% 2014-02-13 09:36:22.639 [error] <0.57.0>@lager_try:start:6 Some message
              [date, " ", time, color, " [",severity,"] <",pid, ">@", module,":", function, ":", line, " ", message, "\e[0m\r\n"]
             }]}.   

log_dir() ->
    LogDir = app_misc:get_env(log_dir, "logs/"),    
    case LogDir of
        "/" ++ _ ->
            filename:join([LogDir, node()]);
        _ ->
            filename:join([app_misc:root_dir(), LogDir, node()])
    end.

crash_log() ->
    filename:join(log_dir(), "server-crash.log").

lager_file_backend_file() ->
    filename:join(log_dir(), "server.log").

%% debug, info, notice, warning, error, critical, alert, emergency
set_loglevel(Level) ->
    case is_console() of
        true ->
            lager:set_loglevel(lager_console_backend, Level);
        false ->
            ignore
    end,
    lager:set_loglevel(lager_file_backend, lager_file_backend_file(), Level).
