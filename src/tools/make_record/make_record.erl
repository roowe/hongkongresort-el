-module(make_record).

-export([start/0]).

-define(DEFAULT_DB_CONF, "/tmp/make_record_db.conf").
-define(DEFAULT_TABLE_CONF, "/tmp/make_record_table.conf").

commands_desc() ->
    [{"table_list", "打印所有支持生成的表名"},
     {"gen [<table_name>]", "生成指定table的record，可以指定多个"},
     {"all", "生成所有表的数据"}
     ].
opt_spec_list() ->
    [
     {db, undefined, "db", {string, ?DEFAULT_DB_CONF}, "数据库连接配置文件"},
     {table, undefined, "table", {string, ?DEFAULT_TABLE_CONF}, "生成record的表的配置文件"},
     {help, $h, "help", undefined, "显示帮助，然后退出"}
    ].
usage() ->
    usage([]).
    %% getopt:usage(opt_spec_list(), "make_record", "<command> [<args>]", commands_desc()),
    %% quit(1).

usage(Opts) ->
    OptSpecList = opt_spec_list(),
    %% 为了可以容易让用户知道明确使用配置的路径，会将传入的配置覆盖掉default配置显示出来
    WithUserOptSpec = lists:foldl(fun({Opt, Val}, AccOptSpecList) ->
                                          Conf = lists:keyfind(Opt, 1, AccOptSpecList),
                                          lists:keyreplace(Opt, 1, AccOptSpecList, setelement(4, Conf, setelement(2, element(4, Conf), Val)))
                                  end, OptSpecList, Opts),   
    getopt:usage(WithUserOptSpec, "make_record", "<command> [<args>]", commands_desc()),
    quit(1).
parse_arguments(CmdLine) ->
    case getopt:parse(opt_spec_list(), CmdLine) of
        {ok, {Opts, [Command | Args]}} ->
            {ok, {list_to_atom(Command), Opts, Args}};
        {ok, {Opts, []}} ->
            {no_command, Opts};
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
            {no_command, ParseOpts} ->
                usage(ParseOpts);
            no_command ->
                usage()
        end,
    %% The reason we don't use a try/catch here is that rpc:call turns
    %% thrown errors into normal return values
    io:format("Opts ~p~n", [Opts]),
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

action(table_list, _Args, Opts) ->
    io:format("This are Support TableList, ~p~n", [table_to_record:tables(Opts)]);
action(gen, Args, Opts) ->
    table_to_record:start([list_to_atom(Table) || Table <- Args], Opts);
action(all, _Args, Opts) ->
    table_to_record:start(table_to_record:tables(Opts), Opts);
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
