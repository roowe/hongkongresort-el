-module(module_misc).
-include("common.hrl").

-export([all_module_attributes/1, cache_modules/0]).

%% 该模块启动或者工具，可以用
%% 其他情况要酌情考虑
all_module_attributes(Name) ->
    Modules =
        lists:usort(
          lists:append(
            [Modules || {App, _, _}   <- application:loaded_applications(),
                        {ok, Modules} <- [app_misc:get_module(App)]])),
    lists:foldl(
      fun (Module, Acc) ->
              case lists:append([Atts || {N, Atts} <- module_attributes(Module),
                                         N =:= Name]) of
                  []   -> Acc;
                  Atts -> [{Module, Atts} | Acc]
              end
      end, [], Modules).

module_attributes(Module) ->
    case catch Module:module_info(attributes) of
        {'EXIT', {undef, [{Module, module_info, _} | _]}} ->
            io:format("WARNING: module ~p not found, so not scanned for boot steps.~n", [Module]),
            [];
        {'EXIT', Reason} ->
            exit(Reason);
        V ->
            V
    end.

cache_modules() ->
    case app_misc:get_module(server) of
        {ok, Modules} ->
            lists:filter(
              fun(Module) ->
                      ModuleStr = atom_to_list(Module),
                      case lists:prefix("cache_", ModuleStr) of
                          true ->
                              case catch Module:module_info(exports) of
                                  {'EXIT', {undef, [{Module, module_info, _} | _]}} ->
                                      io:format("WARNING: cache_module ~p not found.~n", [Module]),
                                      false;
                                  {'EXIT', Reason} ->
                                      exit(Reason);
                                  List ->
                                      lists:member({init,0}, List)
                              end;
                          false ->
                              false
                      end
              end, Modules);
        undefined ->
            []
    end.
        
