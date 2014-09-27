-module(err_misc).

-export([sequence_error/1,
         ensure_ok/2,
         quit/1]).


sequence_error([T]) ->
    T;
sequence_error([{error, _} = Error | _]) -> 
    Error;
sequence_error([_ | Rest]) ->
    sequence_error(Rest).

ensure_ok(ok, _) -> 
    ok;
ensure_ok({error, Reason}, ErrorTag) -> 
    throw({error, {ErrorTag, Reason}}).


%% @doc Halts the emulator returning the given status code to the os.
%% On Windows this function will block indefinitely so as to give the io
%% subsystem time to flush stdout completely.
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
