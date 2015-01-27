-module(data_module_ctl).

-export([start/0]).

commands_desc() ->
    [{"table_list", "打印所有支持生成的表名"},
     {"gen [<table_name>]", "生成指定table的数据，可以指定多个"},
     {"all", "生成所有表的数据"}
     ].
opt_spec_list() ->
    [
     %% {db_host, undefined, "db_host", {string, data_module:default_db_host()}, "数据库Host"},
     %% {db_port, undefined, "db_port", {integer, data_module:default_db_port()}, "数据库端口"},
     %% {db_user, undefined, "db_user", {string, data_module:default_db_user()}, "数据库用户名"},
     %% {db_password, undefined, "db_password", {string, data_module:default_db_password()}, "数据库密码"},
     %% {db_base, undefined, "db_base", {string, data_module:default_db_base()}, "数据库名字"},
     {generate_dir, undefined, "generate_dir", {string, data_module:default_generate_dir()}, "数据的输出目录"},
     {jobs, $j, "jobs", {integer, data_module:default_jobs()}, "生成数据的并发进程数"},
     %% {import_csv, undefined, "import_csv", undefined, "重新从xlsx导入数据到MySQL"},
     %% {update_csv, undefined, "update_csv", undefined, "从Client目录更新csv到/tmp"},
     {csv_root, undefined, "xlsx_csv", {string, data_module:default_csv_root()}, "数据csv目录"},
     {xlsx_root, undefined, "xlsx_root", {string, data_module:default_xlsx_root()}, "数据xlsx目录"},
     {help, $h, "help", undefined, "显示帮助，然后退出"}
    ].
usage() ->
    %% io:format("test 中文 ~p~n", [lists:keyfind(encoding, 1, io:getopts())]),
    getopt:usage(opt_spec_list(), "data_generate", "<command> [<args>]", commands_desc(), standard_io),
    quit(1).
parse_arguments(CmdLine) ->
    case getopt:parse(opt_spec_list(), CmdLine) of
        {ok, {Opts, [Command | Args]}} ->
            {ok, {list_to_atom(Command), Opts, Args}};
        {ok, {_Opts, []}} ->
            no_command;
        Error ->
            io:format("Error ~p~n", [Error]),
            no_command
    end.
start() ->
    ok = io:setopts([{encoding, unicode}]),
    {Command, Opts, Args} =
        case parse_arguments(init:get_plain_arguments()) of
            {ok, Res}  -> 
                Res;
            no_command ->
                usage()
        end,
    %% The reason we don't use a try/catch here is that rpc:call turns
    %% thrown errors into normal return values
    % io:format("Opts ~p~n", [Opts]),
    case catch action(Command, Args, Opts) of
        ok ->
            io:format("done.~n", []),
            quit(0);
        {ok, Info} ->
            io:format("done (~p).~n", [Info]),
            quit(0);        
        Other ->
            io:format("other result ~p~n", [Other]),
            quit(2)
    end.

action(table_list, _Args, _Opts) ->
    io:format("This are Support TableList, ~p~n", [data_module:tables()]);
action(gen, Args, Opts) ->
    data_module:start([list_to_atom(Table) || Table <- Args], Opts);
action(all, _Args, Opts) ->
    data_module:start(data_module:tables(), Opts);
action(Command, Args, Opts) ->
    io:format("Command: ~p Args: ~p Opts: ~p~n", [Command, Args, Opts]),
    invalid_command.


quit(Status) ->
    case os:type() of
        {unix,  _} -> 
            halt(Status);
        {win32, _} -> 
            init:stop(Status),
            receive
            after infinity -> 
                    ok
            end
    end.
