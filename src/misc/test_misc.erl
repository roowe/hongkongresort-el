-module(test_misc).

-export([benchmark/2, benchmark_timestamp/2]).

-export([fprof/2]).

-export([run_mul_process/3,
         run_mul_process/4]).

-define(BENCHMARK_TYPE, [runtime, wall_clock]).

benchmark(TargetName, Target) ->
    [statistics(Type) || Type <-  ?BENCHMARK_TYPE],
    Result = apply1(Target),
    [RunTime, WallClock] = Times = [begin {_, T} = statistics(Type),
                                          T
                                    end || Type <-  ?BENCHMARK_TYPE],
    io:format("~s: ~p(~p)microseconds~n", [TargetName, RunTime, WallClock]),
    {Result, Times}.

benchmark_timestamp(TargetName, Target) ->
    Before = os:timestamp(),
    Result = apply1(Target),
    After = os:timestamp(),
    Times = timer:now_diff(After, Before),
    io:format("~s: ~p microseconds~n", [TargetName, Times]),
    {Result, Times}.

apply1({F, A}) ->
    apply(F, A);
apply1({M, F, A}) ->
    apply(M, F, A);
apply1(F) ->
    F().


fprof(TargetName, Target) ->
    fprof:trace(start),
    Result = apply1(Target),
    fprof:trace(stop),
    fprof:profile(),
    fprof:analyse({dest, TargetName}).


%% N个处理，分ProcessCount个进程去处理Fun
run_mul_process(ProcessCount, N, Fun) ->
    run_mul_process(ProcessCount, N, Fun, []).
run_mul_process(ProcessCount, N, Fun, Args) ->
    PidMRefs = [spawn_monitor(fun() -> 
                                      [apply(Fun, [Id|Args]) || Id <- Ids]
                              end) ||
                   Ids <- ds_misc:list_split(lists:seq(1, N), ProcessCount)], 
    [receive
         {'DOWN', MRef, process, _, normal} -> ok;
         {'DOWN', MRef, process, _, Reason} -> exit(Reason)
     end || {_Pid, MRef} <- PidMRefs],
    ok.
