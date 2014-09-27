-module(statistics).
-include("common.hrl").

-export([
         get_proto_tc/1,
         print_pt_all/0
        ]).

-export([proto_handle_start/1,
         proto_handle_end/1]).


get_proto_tc(ProtocolId) ->
    list_to_atom("proto_tc_" ++ integer_to_list(ProtocolId)).


proto_handle_start(Cmd) ->
    MetricName = get_proto_tc(Cmd),
    case folsom_metrics:notify({MetricName, timer_start}) of
        {error,_,nonexistent_metric} ->
            proto_metric_init(MetricName),
            folsom_metrics:notify({MetricName, timer_start});
        ok ->
            ok
    end.
    
proto_handle_end(Cmd) ->
    MetricName = get_proto_tc(Cmd),
    case folsom_metrics:notify({MetricName, timer_end}) of
        {error,_,nonexistent_metric} ->
            proto_metric_init(MetricName),
            folsom_metrics:notify({MetricName, timer_end});
        ok ->
            ok
    end.


proto_metric_init(MetricName) ->
    ok = folsom_metrics:new_duration(MetricName),
    ok = folsom_metrics:tag_metric(MetricName, protocol).


print_pt_all() ->
    List = folsom_metrics:get_metrics_value(protocol),
    {_NullList, NotNullList} = lists:foldl(fun({Key, Info}, {AccNullList, AccNotNullList}) ->
                                                  case proplists:get_value(count, Info) of
                                                      V when is_integer(V), V > 0 ->
                                                          {AccNullList, [{Key, Info}|AccNotNullList]};
                                                      _->
                                                          {[Key|AccNullList], AccNotNullList}
                                                  end
                                          end, {[], []}, List),
    %io:format("~p~n", [hd(NotNullList)]),    
    {_, ValueList} = hd(NotNullList),

    NameList = lists:foldl(fun({Name, _}, Acc) ->
                                   lists:concat([Acc, "\t", Name])
                           end, "name", ValueList),
    %io:format("~ts~n", [NameList]),
    Str = NameList ++ lists:foldl(fun({Name, Value}, Acc) ->
                                          ValueStr = lists:foldl(fun({_, V}, AccV) ->
                                                                         lists:concat([AccV, "\t", hmisc:term_to_string(V)])
                                                                 end, lists:concat(["\n", Name]), Value),
                                          Acc ++ ValueStr
                                  end, [], NotNullList),
    io:format("~w~n", [Str]),
    File = lists:concat([app_misc:server_root() ++ "logs/proto_tc_", hmisctime:unixtime(), ".csv"]),
    io:format("~ts~n", [File]),
    file:write_file(File, Str).
    %?DEBUG("~p~n", [SourNotNullList]).
