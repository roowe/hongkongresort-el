-module(define_info_to_csv).
-export([start/0]).

start() ->
    OutPut = case init:get_argument(output) of
                 {ok, [[OutPut0]]} ->
                     OutPut0;
                 error ->
                     "/tmp/"
             end,
    filelib:ensure_dir(OutPut),
    HeaderFiles = header_files(),
    AllDataList = 
        lists:foldl(fun(HeaderFile, AccDataList) ->
                            {ok, Bin} = file:read_file(HeaderFile),
                            DefineList = string:tokens(binary_to_list(Bin), "\n"),
                            DataList =
                                lists:foldl(fun(Define, Acc) ->
                                                    Data = get_data_from_define(Define),
                                                    if
                                                        is_tuple(Data) ->
                                                            [Data | Acc];
                                                        true ->
                                                            Acc
                                                    end
                                            end, [], DefineList),
                            DataList ++ AccDataList
                    end, [], HeaderFiles),
    CSV = csv(AllDataList),
    OutPutFile = filename:join(OutPut, "ConfigErrorCodeData.csv"),
    %% io:format("output file ~ts~n", [OutPutFile]),
    Ret = file:write_file(OutPutFile, CSV),
    io:format("Ret ~p~n", [Ret]),
    halt().

csv(AllDataList0) ->
    AllDataList = lists:keysort(2, AllDataList0),
    %% io:format("~p~n", [AllDataList]),
    Header = <<"error_code\terror_define\terror_desc\nint\tstring\tstring\n错误码\t服务器的宏\t描述\n"/utf8>>,
    [Header | [lists:concat([Integer, "\t", Macro, "\t",  Comment, "\n"]) || {Macro, Integer, Comment} <- AllDataList]].

header_files() ->
    %[filename:join([app_misc:server_root(), "include/define_info_0.hrl"])].
    filelib:wildcard(filename:join([app_misc:root_dir(), "include/define_info_*.hrl"])).

get_data_from_define(Define) ->
    case erl_scan:string(Define, 0, [return_comments]) of
        {ok, [{'-', _},
              {atom, _, define},
              {'(', _},
              {var, _, Macro},
              {',', _},
              {integer, _, Integer},
              {')', _},
              {dot, _},
              {comment, _, Comment}],
         _} ->
            NewComment = string:strip(string:strip(Comment, left, $%), left, $ ),
            {Macro, Integer, NewComment};
        _Other ->
            undefined
    end.


