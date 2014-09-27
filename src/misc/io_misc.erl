-module(io_misc).

-export([format/2, format_many/1]).

-export([format_stderr/2]).

-export([local_info_msg/2]).

format(Fmt, Args) -> 
    lists:flatten(io_lib:format(Fmt, Args)).

format_many(List) ->
    lists:flatten([io_lib:format(F ++ "~n", A) || {F, A} <- List]).

format_stderr(Fmt, Args) ->
    io:format(standard_error, Fmt, Args).

%% Execute Fun using the IO system of the local node (i.e. the node on
%% which the code is executing).
with_local_io(Fun) ->
    GL = group_leader(),
    group_leader(whereis(user), self()),
    try
        Fun()
    after
        group_leader(GL, self())
    end.

%% Log an info message on the local node using the standard logger.
%% Use this if rabbit isn't running and the call didn't originate on
%% the local node (e.g. rabbitmqctl calls).
local_info_msg(Format, Args) ->
    with_local_io(fun () -> error_logger:info_msg(Format, Args) end).
