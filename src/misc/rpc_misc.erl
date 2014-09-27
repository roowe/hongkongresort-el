-module(rpc_misc).

-export([call/3, cast/3]).

call(M,F,A) ->
    rpc:call(running_node(), M, F, A).

cast(M,F,A) ->
    rpc:cast(running_node(), M, F, A).


running_node() ->
    Nodes = app_misc:get_env(db_nodes),
    RunningNodes = nodes(),
    ConnectNodes = [Node || Node <- Nodes, lists:member(Node, RunningNodes)],
    hd(ConnectNodes).
