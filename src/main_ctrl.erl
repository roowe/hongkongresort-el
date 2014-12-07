-module(main_ctrl).

-export([start/0]).

-include("common.hrl").
-define(RPC_TIMEOUT, infinity).

commands_desc() ->
    [{"stop", "停止进程"},
     {"stop_app", "关闭application"},
     {"start_app", "打开application"},
     {"started", "节点进程是否存在"}].
opt_spec_list() ->
    [
     {help, $h, "help", undefined, "显示帮助，然后退出"},
     {nodename, undefined, "nodename", {atom, undefined}, "管理节点"}
    ].
usage() ->
    getopt:usage(opt_spec_list(), "server_ctl", "<command> [<args>]", commands_desc()),
    err_misc:quit(1).
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
    NodeName = proplists:get_value(nodename, Opts),
    case net_adm:ping(NodeName) of
        pong ->
            %% timer:sleep(1000), %% wait auto find node
            %% The reason we don't use a try/catch here is that rpc:call turns
            %% thrown errors into normal return values
            %% io:format("Opts ~p~n", [Opts]),
            case catch action(Command, NodeName, Args, Opts) of
                ok ->
                    io:format("done.~n", []),
                    quit(0);
                {ok, Info} ->
                    io:format("~p~n", [Info]),
                    quit(0);        
                Other ->
                    io:format("other result ~p~n", [Other]),
                    quit(1)
            end;
        pang ->
            io:format("nodedown~n", []),
            quit(0)
    end.

action(stop, NodeName, _Args, _Opts) ->
    io:format("Stopping and halting node ~p~n", [NodeName]),
    call(NodeName, {main, stop_and_halt, []});
action(started, NodeName, _Args, _Opts) ->
    call(NodeName, {main, started, []});
action(Command, _NodeName, Args, Opts) ->
    io:format("Command: ~p Args: ~p Opts: ~p~n", [Command, Args, Opts]),
    invalid_command.

call(Node, {Mod, Fun, Args}) ->
    %%rpc_call(Node, Mod, Fun, lists:map(fun list_to_binary/1, Args)).
    rpc_call(Node, Mod, Fun, Args).

rpc_call(Node, Mod, Fun, Args) ->
    rpc:call(Node, Mod, Fun, Args, ?RPC_TIMEOUT).


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
