-module(data_generate).
%% 从数据库加载数据，然后生成erlang代码的工具。后期有需要可以从csv加载，架构上是支持的。
-export([
         data_generate/3, 
         data_generate/5 %% 不推荐，兼容旧代码生成
         %rm_coding_comment/1
        ]).

%% 数据版本
-export([
         load_version_to_ets/1,
         record_version_from_ets/1 %%记录当前的数据版本，写入文件
        ]).

-export([
         ensure_deps_started/0,
         ensure_pool_added/1
        ]).

-export([get_result_str/4]).

-include("define_data_generate.hrl").

-include_lib("emysql/include/emysql.hrl").

-define(DATA_MOUDLE, data_module).
-define(VERSION_FILE, "data_version.txt").
-define(DEPS_APPS, [crypto, emysql]).


get_data_for_fun(single, Data, _) ->
    Data;
get_data_for_fun(all, Data, _) ->
    [{all, Data}];
get_data_for_fun({more, FilterKey}, Data, FieldsWithPos) 
  when is_atom(FilterKey) ->
    get_data_for_fun({more, [FilterKey]}, Data, FieldsWithPos); 
get_data_for_fun({more, FilterKeyList}, Data, FieldsWithPos) ->
    orddict:to_list(
      lists:foldl(fun(RecordData, CatVersion) ->
                          Key = [get_value_by_key(RecordData, FieldsWithPos, FilterKey) || FilterKey <- FilterKeyList],
                          orddict_cons(Key, RecordData, CatVersion)
                  end, orddict:new(), Data)).
    

get_value_by_key(RecordData, FieldsWithPos, Key) ->
    case lists:keyfind(Key, 1, FieldsWithPos) of
        false ->
            throw({not_find, {Key, FieldsWithPos}});
        {Key, Pos} ->
            element(Pos, RecordData)
    end.

get_field_from_field_info(FieldInfo)
  when is_tuple(FieldInfo)->
    element(1, FieldInfo);
get_field_from_field_info(FieldInfo)
  when is_atom(FieldInfo) ->
    FieldInfo.

get_value_by_field_info(RecordData, FieldsWithPos, FieldInfo) ->
    Field = get_field_from_field_info(FieldInfo),
    case get_value_by_key(RecordData, FieldsWithPos, Field) of
        Val when is_binary(Val) ->
            case FieldInfo of
                {_, to_term} ->                                       
                    string:to_lower(binary_to_list(Val));
                {_, F} when is_function(F) ->
                    F(Val);
                {_, to_record, AddRecordName} ->
                    ListVal = binary_to_list(Val),
                    string:to_lower(re:replace(ListVal, "{",
                                               lists:concat(["{", AddRecordName, ", "]), 
                                               [global, {return, list}]));
                _ ->
                    %% io:format("binary_to_list(Val) ~w~n", [Val]),
                    %% io:format("binary_to_list(Val) ~w~n", [<<"<<\"", Val/binary, "\">>">>]),
                    lists:concat(["<<\"", binary_to_list(Val), "\"/utf8>>"])
            end;
        Val ->
            lists:concat([Val])
    end.

%% 获取record的字符串
%% like this: "#base_function_open{type=0,req_lv=1,career=1,open={fuck, [40100101],[]}}"
get_value_list_by_field_info(RecordData, NeedField, FieldsWithPos) 
  when is_atom(NeedField); is_tuple(NeedField)->
    get_value_list_by_field_info(RecordData, [NeedField], FieldsWithPos);
get_value_list_by_field_info(RecordData, NeedFields, FieldsWithPos) 
  when is_list(NeedFields)->
    [get_value_by_field_info(RecordData, FieldsWithPos, FieldInfo) || 
        FieldInfo <- NeedFields].

get_result_str(record, RecordData, NeedFields, FieldsWithPos) ->
    RecordName = element(1, RecordData),
    NeedVals = get_value_list_by_field_info(RecordData, NeedFields, FieldsWithPos),
    FieldValStrs = list_dualmap(fun(FieldInfo, Val) ->
                                        Field = get_field_from_field_info(FieldInfo),
                                        lists:concat([Field, " = ", Val])
                               end, NeedFields, NeedVals),
    AddCommaFieldValStr = string:join(FieldValStrs, ","),
    lists:concat(["#", RecordName, "{", AddCommaFieldValStr, "}"]);
get_result_str(tuple, RecordData, NeedFields, FieldsWithPos) ->
    get_result_str_with_begin_end(RecordData, NeedFields, FieldsWithPos, {"{", "}"});
get_result_str(list, RecordData, NeedFields, FieldsWithPos) ->
    get_result_str_with_begin_end(RecordData, NeedFields, FieldsWithPos, {"[", "]"});
get_result_str(single, RecordData, NeedFields, FieldsWithPos) ->
    get_result_str_with_begin_end(RecordData, NeedFields, FieldsWithPos, {"", ""}).

get_result_str_with_begin_end(RecordData, NeedFields, FieldsWithPos, {Begin, End}) ->
    NeedVals = get_value_list_by_field_info(RecordData, NeedFields, FieldsWithPos),
    AddCommaNeedVals = string:join(NeedVals, ","),
    lists:concat([Begin, AddCommaNeedVals, End]).

get_all_result_str(DataType, RecordDataList, NeedFields, FieldsWithPos) ->
    Vals = [get_result_str(DataType, RecordData, NeedFields, FieldsWithPos) || RecordData <- RecordDataList],
    AddCommaVals = string:join(Vals, ","),
    lists:concat(["[", AddCommaVals, "]"]).

%%-------------------- version --------------------
read_version(GenerateDir) ->
    case file:consult(version_file_path(GenerateDir)) of
        {ok, [V]} ->
            V;
        {error, _} ->
            []
    end.

version_file_path(GenerateDir) ->
    filename:join(GenerateDir, ?VERSION_FILE).

write_version(GenerateDir, VersionList) ->
    write_term_file(version_file_path(GenerateDir), [VersionList]).

check_new_version(GenerateDir, TableName, DataList) ->
    Version = lists:sublist(sha256(DataList), 6),
    case ets:lookup(data_version, TableName) of
        [{_, Version}] ->
            %% 如果文件不存在也需要再次生成
            FileName = src_file_name(GenerateDir, module_name_from_table(TableName)),
            not filelib:is_file(FileName);
        _ ->
            ets:insert(data_version, {TableName, Version}),
            true
    end.

load_version_to_ets(GenerateDir) ->
    ets:new(data_version, [set, public, named_table]),
    VersionList = read_version(GenerateDir),
    [ets:insert(data_version, Version) || Version <- VersionList].
record_version_from_ets(GenerateDir) ->    
    write_version(GenerateDir, lists:sort(ets:tab2list(data_version))),
    ets:delete(data_version).

data_generate(GenerateDir, RecordName, FunConfList) ->
    %% base_login_reward, base_login_reward, ["db_base_login_reward.hrl"]
    %% 如果RecordName, TableName, IncludeFiles遵循上面的规则，就只需要传一个变量，脏活下面帮你干
    IncludeFile = lists:concat(["db_", RecordName, ".hrl"]),
    data_generate(GenerateDir, RecordName, RecordName, [IncludeFile], FunConfList).

data_generate(GenerateDir, RecordName, TableName, IncludeFiles, FunConfList) ->
    ModuleName = module_name_from_table(TableName),
    Data = get_data_from_db(TableName),         %% data for generate
    case check_new_version(GenerateDir, TableName, {[FunConf#generate_conf.fun_name || FunConf <- FunConfList], 
                                                     Data}) of
        true ->            
            data_generate_with_db_data(db_data_to_record(RecordName, Data), 
                                       ModuleName, RecordName,
                                       IncludeFiles, FunConfList, GenerateDir),

            red_print("new version ~p generate.~n", [TableName]);
        false ->
            io:format("~p is same.~n", [TableName]),
            ok
    end.

module_name_from_table(TableName) ->
    lists:concat(["data_", TableName]).

data_generate_with_db_data(Data, ModuleName, RecordName, IncludeFiles, FunConfList, GenerateDir) ->
    %% head src
    CodingStr = "%% coding: utf-8\n", %% for erl_tidy 识别我们代码的编码，否则格式化的话会选择其他编码，导致乱码
    WarningStr = <<"%% Warning:本文件由data_generate自动生成，请不要手动修改\n"/utf8>>,
    ModuleStr = lists:concat(["-module(", ModuleName, ").\n"]),
    ExportAllStr = lists:map(
                     fun(#generate_conf{
                            fun_name = FunName,
                            args_count = ArgsCount,
                            record_conf = RecordConf
                           }) ->
                             if 
                                 RecordConf =:= all ->
                                     NewArgsCount = 0;
                                 true ->
                                     NewArgsCount = ArgsCount
                             end,
                             lists:concat(["-export([", FunName, "/", NewArgsCount, "]).\n"])
                     end, FunConfList),

    IncludeFilesStr = [lists:concat(["-include(\"", File ,"\").\n"]) || 
                          File <- ["common.hrl"|IncludeFiles]],

    %% record info
    FieldsWithPos = 
        case all_record:get_fields(RecordName) of
            [] ->
                throw({not_fields_from_all_record, RecordName});
            RecordFields ->
                lists:zip(RecordFields, lists:seq(2, 1+length(RecordFields)))
        end,
    GenerateFunStrFun = 
        fun (#generate_conf{
                fun_name = FunName, 
                record_conf = ArgsConf, 
                args_count = ArgsCount,
                handle_args_fun = HandleArgsFun, 
                handle_result_fun = HandleResultFun,
                default = Default,
                filter = FilterDataFun,
                warning_not_find = WarningNotFind
               }) ->
                DataForFun = get_data_for_fun(ArgsConf, FilterDataFun(Data), FieldsWithPos),
                FunSrcList = 
                    lists:map(fun
                                  ({all, RecordDataList}) ->
                                      FunArgsStr =
                                          case HandleArgsFun(RecordDataList) of
                                              null ->
                                                  lists:concat([FunName, "() ->\n"]);
                                              OtherArgsStr ->
                                                  lists:concat([FunName, OtherArgsStr])
                                          end,
                                      FunResultStr = 
                                          case HandleResultFun(RecordDataList) of
                                              {DataType, NeedFieldInfo} when
                                                    DataType =:= record;
                                                    DataType =:= tuple;
                                                    DataType =:= list;
                                                    DataType =:= single ->
                                                  get_all_result_str(DataType, RecordDataList, NeedFieldInfo, FieldsWithPos);
                                              OtherResultStr ->
                                                  OtherResultStr
                                          end ++ ".\n",
                                      [FunArgsStr, FunResultStr]; %% iolist 优化
                                  ({KeyList, RecordDataList}) ->
                                      KeyStr = 
                                          if 
                                              length(KeyList) =:= 1 ->
                                                  %% only one element, not with {}
                                                  lists:concat(KeyList);
                                              true ->
                                                  ValsStr = string:join([lists:concat([Key]) || Key <- KeyList], ", "),
                                                  if 
                                                      ArgsCount =:= 1 ->
                                                          %% 如果没有传入参数，就使用{Key1, Key2}
                                                          ["{", ValsStr, "}"];
                                                      true ->
                                                          ValsStr
                                                  end
                                          end,
                                      FunArgsStr = lists:concat([FunName, "(", KeyStr , ") ->\n"]),
                                      FunResultStr = 
                                          case HandleResultFun(RecordDataList) of                                              
                                              {DataType, NeedFieldInfo} when
                                                    DataType =:= record;
                                                    DataType =:= tuple;
                                                    DataType =:= list;
                                                    DataType =:= single ->
                                                  get_all_result_str(DataType, RecordDataList, NeedFieldInfo, FieldsWithPos);
                                              OtherResultStr ->
                                                  OtherResultStr
                                          end,
                                      [FunArgsStr, FunResultStr]; %% iolist 优化
                                  (RecordData) when is_tuple(RecordData)->
                                      FunArgsStr = 
                                          case HandleArgsFun(RecordData) of
                                              Field when is_atom(Field)->
                                                  lists:concat([FunName, "(", get_value_by_key(RecordData, FieldsWithPos, Field), ") ->\n"]);
                                              FieldList when is_list(FieldList), is_atom(hd(FieldList))->
                                                  FieldListStr = string:join([lists:concat([get_value_by_key(RecordData, FieldsWithPos, Field)]) || Field <- FieldList], ", "),
                                                  if 
                                                      ArgsCount =:= 1 ->
                                                          lists:concat([FunName, "({", FieldListStr , "}) ->\n"]);
                                                      true ->
                                                          lists:concat([FunName, "(", FieldListStr , ") ->\n"])
                                                  end;
                                              OtherArgsStr ->
                                                  lists:concat([FunName, OtherArgsStr])
                                          end,
                                      FunResultStr = 
                                          case HandleResultFun(RecordData) of
                                              {DataType, NeedFieldInfo} when
                                                    DataType =:= record;
                                                    DataType =:= tuple;
                                                    DataType =:= list;
                                                    DataType =:= single ->
                                                  get_result_str(DataType, RecordData, NeedFieldInfo, FieldsWithPos);
                                              OtherResultStr ->
                                                  OtherResultStr
                                          end,
                                      [FunArgsStr, FunResultStr] %% iolist 优化
                              end, DataForFun),
                if 
                    ArgsConf =/= all ->
                        ValStr = string:join(lists:map(fun(N) ->
                                                               "Var" ++ integer_to_list(N)
                                                       end, lists:seq(1, ArgsCount)), ", "),
                        WarningPrintStr = if
                                              WarningNotFind =:= true ->
                                                  "?WARNING_MSG(\"" ++  atom_to_list(FunName) ++
                                                      " not find ~p\", [{" ++  ValStr ++ "}]),";
                                              true ->
                                                  ""
                                          end,
                        DefaultFunStr = 
                            lists:concat([FunName, "(", ValStr,
                                          ") -> ", WarningPrintStr, term_to_string(Default), ".\n"]),
                        string:join(FunSrcList ++ [DefaultFunStr], ";\n");
                    true ->
                        %% if ArgsConf is all, only one element, not need default fun
                        hd(FunSrcList)
                end
        end,
    
    AllFunStrList = lists:map(GenerateFunStrFun, FunConfList),
    AllFunStr = string:join(AllFunStrList, "\n"),
    Src = [CodingStr, WarningStr, ModuleStr, ExportAllStr, IncludeFilesStr, AllFunStr],
    FileName = src_file_name(GenerateDir, ModuleName),
    %io:format("FileName ~ts, ~ts", [FileName, Src]),
    ok = file:write_file(FileName, [Src]),
    %% erl_tidy:file是主要瓶颈，单进程下所有表格需要格式化1.6s，不需要格式化仅需要113ms。
    %% 加入了数据版本的记录，大大减少平均使用时间
    erl_tidy:file(FileName, [{backups, false}]),
    %rm_coding_comment(FileName).
    ok.
%% 为了中文的编码能对上，保留原来的方案，暂时不对编码做调整。去掉原来为了erl_tidy能正确处理中文而加的coding。
%rm_coding_comment(GenerateDir) ->
%  os:cmd("cd " ++ GenerateDir ++ " ; sed -i '/%% coding: utf-8/d' *.erl").
%% rm_coding_comment(File) ->
%%     os:cmd("  sed -i '/%% coding: utf-8/d' " ++ File).
        
src_file_name(GenerateDir, ModuleName) ->    
    filename:join(GenerateDir, ModuleName++".erl").

get_data_from_db(TableName) ->
    %% 减少依赖，不用db_base:xxxx，直接拼SQL
    case emysql:execute(db_base, lists:concat(["SELECT * FROM ", TableName])) of
        #result_packet{
           rows = Result
          } ->
            Result;
        Result ->
            throw({db_error, Result})
    end.
    
db_data_to_record(RecordName, DataList) ->
    [list_to_tuple([RecordName|BaseData]) || BaseData <- DataList].

%%-------------------- misc function --------------------
list_dualmap(_F, [], []) ->
    [];
list_dualmap(F, [E1 | R1], [E2 | R2]) ->
    [F(E1, E2) | list_dualmap(F, R1, R2)].

orddict_cons(Key, Value, Dict) ->
    orddict:update(Key, fun (List) -> [Value | List] end, [Value], Dict).

sha256(Data) ->
    to_hex(crypto:hash(sha256, io_lib:format("~w", [Data]))).

to_hex([]) ->
    [];
to_hex(Bin) when is_binary(Bin) ->
    to_hex(binary_to_list(Bin));
to_hex([H|T]) ->
    [to_digit(H div 16), to_digit(H rem 16) | to_hex(T)].

to_digit(N) when N < 10 -> $0 + N;
to_digit(N) -> $a + N-10.

write_term_file(FileName, Terms) ->
    file:write_file(FileName, [io_lib:format("~p.~n", [Term]) || Term <- Terms]).

%% if want to write to file or save to db. iolist also work.
%% Otherwise, you will need lists:flatten or binary_to_list(list_to_binary(..))
term_to_string(Term) ->
    io_lib:format("~p", [Term]).

%%-------------------- ensure --------------------
%% 确保deps的App启动
ensure_deps_started() ->
    [ok = ensure_started(App) || App <- ?DEPS_APPS].

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

ensure_pool_added(Options) ->
    Db = db_base,
    Poolsize = 1,
    User = proplists:get_value(db_user, Options),
    Password = proplists:get_value(db_password, Options),
    Host = proplists:get_value(db_host, Options),
    Port = proplists:get_value(db_port, Options),
    DataBase = proplists:get_value(db_base, Options),
    %% 或许不需要检查pool是否存在，因为我们只会在一开始加一次
    %% 不能直接调用add_pool，因为会泄露db连接，它始终先打开连接再判断是否是合理的，非法的话，并没有关闭连接
    case catch emysql_conn_mgr:wait_for_connection(Db) of
        {'EXIT', pool_not_found} ->
            emysql:add_pool(Db, Poolsize, User, Password, Host, Port, DataBase, utf8);
        Connection ->
            emysql_conn_mgr:pass_connection(Connection)
    end.

red_print(Format, Args) ->
    case os:type() of
        {unix,  _} -> 
            io:format("\e[1;31m" ++ Format ++ "\e[0;38m", Args);
        {win32, _} -> 
            io:format(Format, Args)
    end.
