-module(data_module).

-export([start/0, %% may be for test only, data_module_ctl always transfers options
         start/1,
         start/2,
         recompile_and_commit/0,
         tables/0]).

-export([import_from_csv/1]).

-export([
         default_db_host/0,
         default_db_port/0,
         default_db_user/0,
         default_db_password/0,
         default_db_base/0,
         default_generate_dir/0,
         default_jobs/0,
         default_xlsx_root/0
        ]).

-export([fprof/0]).

%% -export([
%%          get_fields/1 %% not public 内部接口 获取字段
%%         ]).
%% for apply(?MODULE, Fun, [])
-compile(nowarn_unused_function).
%% 不会报下面函数没有被用到的警告
-export([
         base_error_list/0
        ]).

-include("define_data_generate.hrl").

%% data header file
-include("db_base_error_list.hrl").

-define(DEFAULT_OPTIONS, [
                          {db_host, "127.0.0.1"},
                          {db_port, 3306},
                          {db_user, "root"}, 
                          {db_password, "root"},
                          {db_base, "hongkongresort_base"},
                          {generate_dir, "src/data/"},
                          {jobs, 2},
                          {xlsx_root, "/home/roowe/happytree/触动足球/peizhi"}
                          ]).


fprof() ->
    fprof:trace(start),
    start(),
    fprof:trace(stop),
    fprof:profile(),
    fprof:analyse({dest, "data_module.txt"}).

%% 优化：多进程生成
mul_process(AllTables, Options) ->
    GenerateDir = generate_dir(Options),
    Jobs = jobs(Options),
    GenerateFun =
        fun(TableName) ->
                apply(data_generate, data_generate, [GenerateDir|?MODULE:TableName()])
        end,
    PidMRefs = [spawn_monitor(fun () -> 
                                      [GenerateFun(Table) || Table <- Tables]
                              end) ||
                   Tables <- list_split(AllTables, Jobs)],
    [receive
         {'DOWN', MRef, process, _, normal} -> ok;
         {'DOWN', MRef, process, _, Reason} -> exit(Reason)
     end || {_Pid, MRef} <- PidMRefs],
    ok.

start() ->
    start(tables(), ?DEFAULT_OPTIONS).
start(Tables) ->
    start(Tables, ?DEFAULT_OPTIONS).

start(TableName, Options) 
  when is_atom(TableName) ->
    start([TableName], Options);
start(TableNameList, Options) ->
    %% io:format("Options ~p~n", [Options]),
    print_options_info(Options),
    GenerateDir = generate_dir(Options),
    data_generate:load_version_to_ets(GenerateDir),
    AllowTableList = sets:to_list(
                       sets:intersection(
                         sets:from_list(TableNameList), 
                         sets:from_list(tables()))),

    data_generate:ensure_deps_started(),
    data_generate:ensure_pool_added(Options),    
    import_from_csv(TableNameList, Options),   
    local_info_msg("AllowTableList ~p~n", [AllowTableList]),
    mul_process(AllowTableList, Options),
    case TableNameList -- AllowTableList of
        [] ->
            ignore;
        IllegalTableList ->
            io:format("<<warning>> these tables ~p are not generated. because these are not in ~p~n", 
                      [IllegalTableList, tables()])
    end,
    data_generate:record_version_from_ets(GenerateDir),
    %data_generate:rm_coding_comment(GenerateDir),
    ok.

%% 手动更新不需要生成的配置表
import_from_csv(TableNameList) ->
    import_from_csv(TableNameList, [import_csv|?DEFAULT_OPTIONS]).

import_from_csv(TableNameList, Options) ->
    case lists:member(import_csv, Options) of
        false ->
            ingore;
        true ->
            local_info_msg("import from csv start ~n", []),    
            User = proplists:get_value(db_user, Options),
            Password = proplists:get_value(db_password, Options),
            Host = proplists:get_value(db_host, Options),
            Port = proplists:get_value(db_port, Options),
            DataBase = proplists:get_value(db_base, Options),
            XlsxRoot = proplists:get_value(xlsx_root, Options),
            cmd_print_ret("cd " ++ XlsxRoot ++ " && svn up"),
            ImportFromCsvFun = 
                fun(Table) ->

                        Cmd = "cd " ++ XlsxRoot ++ " && "++ 
                            lists:concat([" xlsx2csv -i ", Table, ".xlsx /tmp/", Table, ".csv -d tab "]),
                        cmd_print_ret(Cmd),
                        LoadCmd = 
                            lists:concat(["mysql --local-infile -h", Host, " -P", Port, " -u", User, " -p", Password, " -e \'USE ", DataBase, ";DELETE FROM ", Table, ";LOAD DATA LOCAL INFILE \"/tmp/", Table, ".csv\" INTO TABLE ", Table, " FIELDS OPTIONALLY ENCLOSED BY \"\\\"\" ESCAPED BY \"\\\\\"   IGNORE 1 LINES ", fields_for_load(Table), ";\'"]),
                        cmd_print_ret(LoadCmd)

                end,
            [ImportFromCsvFun(Table) || Table <- TableNameList -- [base_error_list, base_mail]],
            local_info_msg("import from csv end ~n", [])
    end.    
    


%% (id, @in, @`desc`, reward);
fields_for_load(Table) ->
    RecordFields = all_record:get_fields(Table),
    {ok, Fd} = file:open(lists:concat(["/tmp/", Table, ".csv"]), read),
    {ok, Line} = file:read_line(Fd),
    ok = file:close(Fd),
    CsvFields = string:tokens(Line, "\t"),
    FixFields = [begin
                     RemoveNField = string:strip(Field, both, $\n),
                     case lists:member(list_to_atom(RemoveNField), RecordFields) of
                         true ->
                             "`"++ RemoveNField ++"`";
                         false ->
                             "@`"++ RemoveNField ++"`"
                     end
                 end || Field <- CsvFields],
    "(" ++ string:join(FixFields, ",") ++ ")".


cmd_print_ret(Cmd) ->
    os:cmd(Cmd).
     %local_info_msg("Cmd: ~ts~n", [Cmd]),
     %Ret = os:cmd(Cmd),
     %local_info_msg("Ret: ~p~n", [Ret]).

%% -------------------- for data_module_ctl --------------------

tables() ->
    Functions = ?MODULE:module_info(exports),
    Tables = [ FName || {FName, _} <- lists:filter(
                                        fun ({FName, 0}) -> 
                                                case atom_to_list(FName) of
                                                    "base_" ++ _ ->
                                                        true;
                                                    _ ->
                                                        false
                                                end;
                                            ({_, _}) ->
                                                false
                                        end, Functions)],
    Tables.

default_db_host() ->
    proplists:get_value(db_host, ?DEFAULT_OPTIONS).

default_db_port() ->
    proplists:get_value(db_port, ?DEFAULT_OPTIONS).

default_db_user() ->
    proplists:get_value(db_user, ?DEFAULT_OPTIONS).

default_db_password() ->
    proplists:get_value(db_password, ?DEFAULT_OPTIONS).

default_db_base() ->
    proplists:get_value(db_base, ?DEFAULT_OPTIONS).

default_generate_dir() ->
    generate_dir(?DEFAULT_OPTIONS).

default_xlsx_root() ->
    proplists:get_value(xlsx_root, ?DEFAULT_OPTIONS).

default_jobs() ->
    proplists:get_value(jobs, ?DEFAULT_OPTIONS).



%% -------------------- 处理选项 --------------------
   
generate_dir(Options) ->
    %% "/home/roowe/happytree/server_p02/ebin/main.beam"
    Root = server_root(),
    case proplists:get_value(generate_dir, Options) of
        "/" ++ _ = Path ->
            Path;
        RelativePath ->
            filename:join(Root, RelativePath)
    end.

server_root() ->
    app_misc:root_dir().

remove_suffix_ebin(MainPath) ->
    Detal = length(MainPath) - length("ebin/main.beam"),
    lists:sublist(MainPath, Detal).

jobs(Options) ->
    proplists:get_value(jobs, Options).

print_options_info(Options) ->
    io:format("--------------------选项信息--------------------~n", []),
    io:format("数据库Host ~s~n", [proplists:get_value(db_host, Options)]),
    io:format("数据库端口 ~p~n", [proplists:get_value(db_port, Options)]),
    io:format("数据库用户名 ~s~n", [proplists:get_value(db_user, Options)]),
    io:format("数据库密码 ~s~n", [proplists:get_value(db_password, Options)]),
    io:format("数据库名字 ~s~n", [proplists:get_value(db_base, Options)]),
    io:format("数据的输出目录 ~s~n", [proplists:get_value(generate_dir, Options)]),
    io:format("生成数据的并发进程数 ~p~n", [proplists:get_value(jobs, Options)]),
    io:format("------------------------------------------------~n", []).
%%--------------------每张表的生成函数--------------------

base_error_list() ->
    Fields = record_info(fields, base_error_list), 
    [base_error_list, 
     [default_get_generate_conf(Fields, error_code)]].
%% --------------------recompile and git commit--------------------
recompile_and_commit() ->
    Root = server_root(),
    [_Node, IP] = string:tokens(atom_to_list(node()), "@"),
    if
        IP =:= "192.168.1.149" ->
            %% 149 git version is old. not support --no-edit.
            Cmd = "cd " ++ Root ++"; rebar compile; git add src/data/*erl src/data/*txt; git commit -m 'data_generate auto commit'; git pull ; git push origin master; ";
        true ->
            Cmd = "cd " ++ Root ++"; rebar compile; git add src/data/*erl src/data/*txt; git commit -m 'data_generate auto commit'; git pull --no-edit ; git push origin master; "
    end,
    local_info_msg("recompile_and_commit cmd ~p~n", [Cmd]),
    Result = os:cmd(Cmd),
    local_info_msg("recompile_and_commit ~p~n", [Result]),
    ok.

%%-------------------- 内部函数 --------------------

default_get_generate_conf(GetFunFields, Key) ->
    default_get_generate_conf(GetFunFields, Key, []).

default_get_generate_conf(GetFunFields, Key, Default) ->  
    #generate_conf{
       fun_name = get, 
       record_conf = single, 
       handle_args_fun = fun(_RecordData) ->
                                 Key
                         end, 
       handle_result_fun = fun(_RecordData) ->
                                   {record, GetFunFields}
                           end,
       default = Default
      }.

default_all_generate_conf(Key) ->  
    #generate_conf{
       fun_name = all, 
       record_conf = all, 
       handle_args_fun = fun(_RecordData) ->
                                 null
                         end, 
       handle_result_fun = fun(_RecordData) ->
                                   {single, Key}
                           end
      }.
%% FilterKeyInfo can one or more
default_more_generate_conf(FunName, FilterKeyInfo, Id) ->
    #generate_conf{
       fun_name = FunName, 
       record_conf = {more, FilterKeyInfo}, 
       handle_result_fun = fun(_RecordData) ->
                                   {single, Id}
                           end
      }.

%% 加上转term的标志
trans_to_term(Fields, ToTermField) 
  when is_atom(ToTermField) ->
    trans_to_term(Fields, [ToTermField]);
trans_to_term(Fields, ToTermFieldList) ->
    lists:map(fun
                  (Field) when is_atom(Field) ->
                      case lists:member(Field, ToTermFieldList) of
                          true ->
                              {Field, to_term};
                          false ->
                              Field
                      end;
                  (Field) ->
                      Field
              end, Fields).

%% 将一些字段添加上转record标志
trans_to_record(Fields, {ToRecordField, RecordName}) 
  when is_atom(ToRecordField),
       is_atom(RecordName)->
    trans_to_record(Fields, [{ToRecordField, RecordName}]);
trans_to_record(Fields, ToRecordFieldList) ->
    lists:map(fun
                  (Field) when is_atom(Field) ->
                      case lists:keyfind(Field, 1, ToRecordFieldList) of
                          {_, RecordName} ->
                              {Field, to_record, RecordName};
                          false ->
                              Field
                      end;
                  (Field) ->
                      Field
              end, Fields).

%% 将一些字段添加上Fun
trans_to_fun(Fields, {Field, Fun}) 
  when is_atom(Field),
       is_function(Fun)->
    trans_to_record(Fields, [{Field, Fun}]);
trans_to_fun(Fields, ToFunFieldList) ->
    lists:map(fun
                  (Field) when is_atom(Field) ->
                      case lists:keyfind(Field, 1, ToFunFieldList) of
                          {_, Fun} ->
                              {Field, Fun};
                          false ->
                              Field
                      end;
                  (Field) ->
                      Field
              end, Fields).

%% 去掉一些字段
del_fields(Fields, DelField) 
  when is_atom(DelField)->
    del_fields(Fields, [DelField]);
del_fields(Fields, DelFieldList) ->
    lists:filter(fun
                     (Field) when is_atom(Field) ->
                         not lists:member(Field, DelFieldList);                     
                     (_) ->
                         true 
                 end, Fields).

list_split(L, N) -> 
    list_split0(L, [[] || _ <- lists:seq(1, N)]).

list_split0([], Ls) -> 
    Ls;
list_split0([I | Is], [L | Ls]) -> 
    list_split0(Is, Ls ++ [[I | L]]).


bitstring_to_term(undefined) -> 
    undefined;
bitstring_to_term(BitString) ->
    string_to_term(binary_to_list(BitString)).

string_to_term(String) ->
    case erl_scan:string(String++".") of
        {ok, Tokens, _} ->
            case erl_parse:parse_term(Tokens) of
                {ok, Term} -> 
                    Term;
                Err -> 
                    throw({err_string_to_term, Err})
            end;
        Error ->
            throw({err_string_to_term, Error})
    end.


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
