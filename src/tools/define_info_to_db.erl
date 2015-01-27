-module(define_info_to_db).
%% -include_lib("emysql/include/emysql.hrl").
%% -include("db_base_error_list.hrl").

%% -export([start/0]).

%% start() ->
%%     ensure_deps_started(),
%%     %% ensure_pool_added([{db_user, "mroot"}, 
%%     %%                    {db_password, "mroot"},
%%     %%                    {db_host, "192.168.1.149"}]), 
%%     ensure_pool_added([]), 
%%     HeaderFiles = header_files(),
%%     AllDataList = 
%%         lists:foldl(fun(HeaderFile, AccDataList) ->
%%                             {ok, Bin} = file:read_file(HeaderFile),
%%                             DefineList = string:tokens(binary_to_list(Bin), "\n"),
%%                             DataList =
%%                                 lists:foldl(fun(Define, Acc) ->
%%                                                     Data = get_data_from_define(Define),
%%                                                     if
%%                                                         is_tuple(Data) ->
%%                                                             [Data | Acc];
%%                                                         true ->
%%                                                             Acc
%%                                                     end
%%                                             end, [], DefineList),
%%                             DataList ++ AccDataList
%%                     end, [], HeaderFiles),

%%     #result_packet{
%%        rows = OldDataList
%%       } = emysql:execute(db_base, "SELECT * FROM base_error_list"),
    
%%     SQL = sql(AllDataList, to_base_error_list(OldDataList)),
%%     io:format("~ts~n", [SQL]),
%%     Ret = emysql:execute(db_base, SQL, 100000),
%%     io:format("Ret ~p~n", [Ret]),
%%     ok.


%% to_base_error_list(List) ->
%%     [list_to_tuple([base_error_list|BaseErrorList]) ||
%%         BaseErrorList <- List].

%% header_files() ->
%%     %[filename:join([app_misc:server_root(), "include/define_info_0.hrl"])].
%%     filelib:wildcard(filename:join([app_misc:root_dir(), "include/define_info_*.hrl"])).

%% %% UPDATE `base_error_list` SET `error_desc`="test", `error_define`="test" WHERE `error_code`=13001
%% %% INSERT INTO `base_error_list` (`error_code`, `error_define`, `error_desc`) VALUES(2333,"test","orz")
%% %% DELETE FROM `base_error_list` WHERE `error_code` IN (23,222)
%% sql(List, BaseErrorLists) ->
%%     ValFun = fun(Macro, Integer, Comment) ->
%%                      NewComment = string:strip(string:strip(Comment, left, $%), left, $ ),
%%                      case lists:keyfind(Integer, #base_error_list.error_code, BaseErrorLists) of
%%                          false ->
%%                              io:format("insert id ~p~n", [Integer]),
%%                              lists:concat(["INSERT INTO `base_error_list` (`error_code`, `error_define`, `error_desc`) VALUES ", "(\"", Integer, "\",\"", Macro , "\",\"", NewComment, "\");\n"]);
%%                          BaseErrorList ->
%%                              case equal({Macro, Integer, NewComment}, BaseErrorList) of
%%                                  true ->
%%                                      "";
%%                                  false ->
%%                                      lists:concat(["UPDATE `base_error_list` SET `error_define`=\"",Macro,"\", `error_desc`=\"",NewComment,"\" WHERE `error_code`=", Integer, ";\n"])
%%                              end
%%                      end  
%%              end,
%%     InsertOrUpdateSQL = [ValFun(Macro, Integer, Comment) ||
%%                             {Macro, Integer, Comment} <- List],
    
%%     DelIds = [ErrorCode || #base_error_list{
%%                                                error_code = ErrorCode
%%                                               } <- BaseErrorLists] -- [Integer || {_Macro, Integer, _Comment} <- List],    
%%     io:format("DelIds ~p~n", [DelIds]),
%%     case DelIds of
%%         [] ->
%%             InsertOrUpdateSQL;
%%         _ ->
%%             DelIdStrs = [integer_to_list(Id) || Id <- DelIds],
%%             ["DELETE FROM `base_error_list` WHERE `error_code` IN (" ++ string:join(DelIdStrs, ",") ++ ");\n" | InsertOrUpdateSQL]
%%     end.
%% %% {base_error_list,520,<<73,78,70,80,95,71,69,84,95,82,69,87,65,82,68>>,<<32,230,129,173,229,150,156,230,130,168,232,142,183,229,190,151,37,115>>,0} {'INFP_GET_REWARD',520,[37,37,32,230,129,173,229,150,156,230,130,168,232,142,183,229,190,151,37,115]}
%% equal({Macro, _Integer, Comment}, #base_error_list{
%%                                     error_desc = ErrorDesc,
%%                                     error_define = ErrorDefine
%%                                    }) ->
%%     atom_to_binary(Macro, utf8) =:= ErrorDefine andalso
%%         list_to_binary(Comment) =:= ErrorDesc.

%% get_data_from_define(Define) ->
%%     case erl_scan:string(Define, 0, [return_comments]) of
%%         {ok, [{'-', _},
%%               {atom, _, define},
%%               {'(', _},
%%               {var, _, Macro},
%%               {',', _},
%%               {integer, _, Integer},
%%               {')', _},
%%               {dot, _},
%%               {comment, _, Comment}],
%%          _} ->
%%             {Macro, Integer, Comment};
%%         _Other ->
%%             undefined
%%     end.


%% %%-------------------- ensure --------------------
%% %% 确保deps的App启动
%% ensure_deps_started() ->
%%     [ok = ensure_started(App) || App <- [crypto, emysql]].

%% ensure_started(App) ->
%%     case application:start(App) of
%%         ok ->
%%             ok;
%%         {error, {already_started, App}} ->
%%             ok
%%     end.

%% ensure_pool_added(Options) ->
%%     Db = db_base,
%%     Poolsize = 1,
%%     User = proplists:get_value(db_user, Options, "root"),
%%     Password = proplists:get_value(db_password, Options, "root"),
%%     Host = proplists:get_value(db_host, Options, "127.0.0.1"),
%%     Port = proplists:get_value(db_port, Options, 3306),
%%     DataBase = proplists:get_value(db_base, Options, "hongkongresort_base"),
%%     case catch emysql:add_pool(Db, Poolsize, User, Password, Host, Port, DataBase, utf8) of
%%         {'EXIT', pool_already_exists} ->
%%             ok;
%%         Other ->
%%             Other
%%     end.
