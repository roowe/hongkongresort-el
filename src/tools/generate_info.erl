-module(generate_info).

-export([start/0]).

-define(ERROR_CODE_CSV, app_misc:root_dir() ++ "/config/base_error_code.csv").

start() ->    
    List = csv_parse:parse(?ERROR_CODE_CSV, all_record:get_fields(base_error_code)),
    Dict = lists:foldl(fun([ErrorCode|_]=Data, AccDict) ->
                               ds_misc:dict_cons(ErrorCode div 1000, Data, AccDict)
                       end, dict:new(), List),
    dict:map(fun(K, SubList) ->
                     try 
                         gen(K, SubList)
                     catch 
                         _:_ ->
                             io:format("~p~n", [erlang:get_stacktrace()])
                     end
             end, Dict),
    ok.

gen(N, List) ->
    Src = 
        lists:foldl(fun([Code, Define, Comment], Acc) ->
                          <<"-define(", Define/binary, ", ", (integer_to_binary(Code))/binary, "). %% ", Comment/binary,"\n", 
                            Acc/binary>>
                    end, <<>>, List),
    Head = <<"-ifndef(DEFINE_INFO_", (integer_to_binary(N))/binary,
             "_HRL).\n-define(DEFINE_INFO_", (integer_to_binary(N))/binary, "_HRL, true).\n\n">>,    
    Bin0 = <<Head/binary, Src/binary, "-endif.\n">>,
    BinVersion = crypto_misc:md5(Bin0),
    VersionSrc = <<"%% version : ", BinVersion/binary, "\n">>,
    Bin = <<VersionSrc/binary, "%% 自动生成，请勿手动修改，避免重新生成的时候，造成不必要的数据丢失\n"/utf8, Bin0/binary>>,
    FileName = app_misc:root_dir() ++ "/include/define_info_" ++ integer_to_list(N) ++ ".hrl",
    %% 加版本号的原因是因为，减少不必要的重新编译
    case file:open(FileName, [read]) of
        {ok, FD} ->
            VersionSrcList = binary_to_list(VersionSrc),
            case file:read_line(FD) of                
                {ok, VersionSrcList} ->
                    %% io:format("~p same~n", [N]),
                    ok;
                _ ->
                    io:format("~p new~n", [N]),
                    file:write_file(FileName, Bin)
            end,
            file:close(FD);
        _ ->
            io:format("~p new~n", [N]),            
            file:write_file(FileName, Bin)
    end.

