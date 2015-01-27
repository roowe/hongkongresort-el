-module(data_module).

-export([start/0, %% may be for test only, data_module_ctl always transfers options
         start/1,
         start/2,
         tables/0]).

%% -export([import_from_csv/1]).

-export([
         default_db_host/0,
         default_db_port/0,
         default_db_user/0,
         default_db_password/0,
         default_db_base/0,
         default_generate_dir/0,
         default_jobs/0,
         default_xlsx_root/0,
         default_csv_root/0
        ]).

-export([fprof/0]).

%% -export([
%%          get_fields/1 %% not public 内部接口 获取字段
%%         ]).
%% for apply(?MODULE, Fun, [])
-compile(nowarn_unused_function).
%% 不会报下面函数没有被用到的警告

-export([
         base_error_code/0
        ]).

-include("define_data_generate.hrl").

%% data header file
-include("db_base_error_code.hrl").

-define(DEFAULT_OPTIONS, [
                          %% {db_host, "127.0.0.1"},
                          %% {db_port, 3306},
                          %% {db_user, "root"}, 
                          %% {db_password, "root"},
                          %% {db_base, "p04_base"},
                          {generate_dir, app_misc:root_dir() ++ "/src/data/"},
                          {jobs, erlang:system_info(schedulers)},
                          {xlsx_root, app_misc:root_dir() ++ "/common_svn/Excel/"},
                          {csv_root, app_misc:root_dir() ++ "/.data_tmp/"}
                          ]).


fprof() ->
    fprof:trace(start),
    start(),
    fprof:trace(stop),
    fprof:profile(),
    fprof:analyse({dest, "data_module.txt"}).

%% 优化：多进程生成
mul_process(AllTables, Options) ->    
    Jobs = proplists:get_value(jobs, Options),
    GenerateFun =
        fun(TableName) ->
                apply(data_generate, data_generate, [Options|?MODULE:TableName()])
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
    print_options_info(Options),
    GenerateDir = proplists:get_value(generate_dir, Options),
    data_generate:load_version_to_ets(GenerateDir),
    AllowTableList = sets:to_list(
                       sets:intersection(
                         sets:from_list(TableNameList), 
                         sets:from_list(tables()))),
    %% data_generate:ensure_deps_started(),
    %% data_generate:ensure_pool_added(Options),    
    %% import_from_csv(TableNameList, Options),  
    %% update_csv(TableNameList, Options),
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
    proplists:get_value(generate_dir, ?DEFAULT_OPTIONS).

default_xlsx_root() ->
    proplists:get_value(xlsx_root, ?DEFAULT_OPTIONS).

default_csv_root() ->
    proplists:get_value(csv_root, ?DEFAULT_OPTIONS).

default_jobs() ->
    proplists:get_value(jobs, ?DEFAULT_OPTIONS).



%% -------------------- 处理选项 --------------------

print_options_info(Options) ->
    io:format("--------------------选项信息--------------------~n", []),
    io:format("csv目录 ~s~n", [proplists:get_value(csv_root, Options)]),
    io:format("xlsx目录 ~s~n", [proplists:get_value(xlsx_root, Options)]),
    io:format("数据的输出目录 ~s~n", [proplists:get_value(generate_dir, Options)]),
    io:format("生成数据的并发进程数 ~p~n", [proplists:get_value(jobs, Options)]),
    io:format("------------------------------------------------~n", []).
%%--------------------每张表的生成函数--------------------
base_error_code() ->
    Fields = record_info(fields, base_error_code), 
    [base_error_code, 
     [default_get_generate_conf(Fields, error_code)]].

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
