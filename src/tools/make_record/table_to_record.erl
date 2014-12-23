-module(table_to_record).

-export([
         start/2,
         tables/1
        ]). 

-include_lib("emysql/include/emysql.hrl").
-define(VERSION_FILE, "make_record_version.txt").

tables(Opts) ->
    lists:map(fun({Table, _}) ->
                      Table;
                 (Table) ->
                      Table
              end,  get_table_config(table_conf_file(Opts))).

db_conf_file(Opts) ->
    proplists:get_value(db, Opts).

table_conf_file(Opts) ->
    proplists:get_value(table, Opts).

version_file(ConfFile) ->
    Dir = filename:dirname(ConfFile),
    filename:join(Dir, ?VERSION_FILE).

get_table_config(ConfFile) ->
    lists:map(fun({TableName, RecordName}) ->
                      {TableName, RecordName};
                 (TableName) when is_atom(TableName) ->
                      {TableName, TableName}
              end, get_config(ConfFile, get_table_config)).

get_db_config(ConfFile) ->
    get_config(ConfFile, get_db_config).

get_config(ConfFile, Type) ->
    case file:consult(ConfFile) of
        {ok, [V]} ->
            V;
        Other ->
            throw({Type, ConfFile, Other})
    end.

ensure_mysql(DbConf) ->
    ok = application:start(crypto),
    ok = application:start(emysql),
    Host = proplists:get_value(host, DbConf),
    Port = proplists:get_value(port, DbConf),
    User = proplists:get_value(user, DbConf),
    Password = proplists:get_value(password, DbConf),
    emysql:add_pool(db001, 1, User, Password, Host, Port, "information_schema", utf8).

start(Tables, Opts) ->
    DbConf = get_db_config(db_conf_file(Opts)),
    ok = ensure_mysql(DbConf),
    DbList = proplists:get_value(dblist, DbConf),
    IncludePath = proplists:get_value(include_path, DbConf),
    SrcPath = proplists:get_value(src_path, DbConf),
    %% DbList和IncludePath是因为每个使用者都可能不一样，so没放在table conf那边

    TableConfFile = table_conf_file(Opts),   
    TableConfs = get_table_config(TableConfFile),
    %% io:format("~p~n", [TableConfs]),
    NeedGenerateTableConfs = filter_need_generate(TableConfs, Tables),

    TableDbHash = lists:foldl(fun(Db, Acc) ->
                                      emysql:execute(db001, lists:concat(["USE ", Db])),
                                      case get_db_result("SHOW TABLES") of
                                          [] ->
                                              throw({not_db_info, Db});
                                          TableList ->
                                              lists:map(fun([Table]) ->
                                                                {binary_to_atom(Table, latin1), Db}
                                                        end, TableList)
                                      end ++ Acc
                end, [], DbList),
    VersionFile = version_file(TableConfFile),
    load_version_info(VersionFile),
    tables_to_record(NeedGenerateTableConfs, IncludePath, TableDbHash),
    table_record_info(TableConfs, SrcPath),
    record_version_from_ets(VersionFile),
    ok.

%% 选取需要生成的表的配置
filter_need_generate(TableConfs, Tables) ->
    lists:foldl(fun(Table, AccTableConfs) ->
                        case lists:keyfind(Table, 1, TableConfs) of
                            false ->
                                io:format("can't find ~p, please add it to db~n", [Table]),
                                AccTableConfs;
                            Conf ->
                                [Conf|AccTableConfs]
                        end
                end, [], Tables).
%%-------------------- 数据版本 --------------------
read_version(VersionFile) ->
    case file:consult(VersionFile) of
        {ok, [V]} ->
            V;
        {error, _} ->
            []
    end.

write_version(VersionFile, VersionList) ->
    write_term_file(VersionFile, [VersionList]).

write_term_file(FileName, Terms) ->
    file:write_file(FileName, [io_lib:format("~p.~n", [Term]) || Term <- Terms]).

load_version_info(VersionFile) ->
    ets:new(record_version, [set, public, named_table]),
    VersionList = read_version(VersionFile),
    [ets:insert(record_version, Version) || Version <- VersionList].

record_version_from_ets(VersionFile) ->    
    write_version(VersionFile, lists:sort(ets:tab2list(record_version))),
    ets:delete(record_version).

check_db_new_version(IncludePath, TableName, DataList) ->
    check_new_version(TableName, DataList, record_file_name(IncludePath, TableName)).

check_new_version(Key, DataList, FileName) ->
    Version = lists:sublist(sha256(DataList), 6),
    case ets:lookup(record_version, Key) of
        [{_, Version}] ->
            %% io:format("~p~n", [Version]),
            %% 如果文件不存在也需要再次生成
            not filelib:is_file(FileName);
        _ ->
            ets:insert(record_version, {Key, Version}),
            true
    end.
%% ----------------------------------------

tables_to_record(TableConfs, IncludePath, TableDbHash) ->
    io:format("generate include path ~s~n", [IncludePath]),
    lists:foreach(
      fun({TableName, RecordName})-> 
              case proplists:get_value(TableName, TableDbHash) of
                  undefined ->
                      io:format("can't find ~p, please add it to db~n", [TableName]);
                  Db ->
                      DbFieldsInfo = get_data_from_db(lists:concat([Db, ".", TableName])),
                      case check_db_new_version(IncludePath, TableName, DbFieldsInfo) of
                          true ->
                              FieldsInfo = get_need_field_info(DbFieldsInfo),
                              WarningStr = <<"%% Warning:本文件由make_record自动生成，请不要手动修改\n"/utf8>>,
                              IfNDefStr = lists:concat(["-ifndef(DB_", 
                                                        string:to_upper(atom_to_list(TableName)),
                                                        "_HRL).\n"]),
                              DefStr = lists:concat(["-define(DB_",
                                                     string:to_upper(atom_to_list(TableName)),
                                                     "_HRL, true).\n"]),
                              TableToReocrdComment = lists:concat(["%% ", TableName, " => ", RecordName, "\n"]),                     
                              RecordSrc = generate_record_source(RecordName, FieldsInfo),                      
                              EndIfStr = "-endif.\n",
                              Src = [WarningStr, IfNDefStr, DefStr, TableToReocrdComment, RecordSrc, EndIfStr],

                              ok = file:write_file(record_file_name(IncludePath, TableName), [Src]),
                              red_print("~s generate ok~n", [TableName]);
                          false ->
                              io:format("~s same~n", [TableName])
                      end
              end  
      end, TableConfs).

record_file_name(IncludePath, TableName) ->
    filename:join(IncludePath, lists:concat(["db_", TableName,".hrl"])).

get_need_field_info(DbFieldsInfo) ->
    %% Field | Type | Collation | Null | Key | Default | Extra | Privileges | Comment
    %% init_exp | int(11) | NULL | NO |     | NULL |  | select,insert,update,references | 初始拥有的经验
    lists:map(fun(FieldInfo) ->
                      [ColumnName, Type,
                       _, _, _,
                       ColumnDefault,
                       _, _, 
                       ColumnComment] = FieldInfo,
                      {ColumnName, 
                       record_default_val(Type, ColumnDefault), 
                       ColumnComment}
              end, DbFieldsInfo).

get_data_from_db(FullTableName) ->    
    get_db_result(["SHOW FULL FIELDS FROM  ", FullTableName]).

get_db_result(SQL) ->
    case emysql:execute(db001, SQL) of
        #result_packet{
           rows = Result
          } ->
            Result;
        Result ->
            throw({db_error, Result})
    end.

record_default_val(_Type, undefined) ->
    <<>>;
record_default_val(<<"timestamp">>, _) ->
    <<>>;
record_default_val(Type, Default) ->
    case mysql_data_type(Type) of
        integer ->
            %% 优化
            Default;
        _ ->
            %% this is a brute check.
            %% term如果是integer, list, tuple就返回true，其他结果或者失败都是false
            case is_term(binary_to_list(Default)) of
                true ->
                    Default;
                false ->
                    <<"<<\"", Default/binary, "\"/utf8>>">>
            end
    end.

mysql_data_type(<<"bigint", _/binary>>) ->
    integer;
mysql_data_type(<<"smallint", _/binary>>) ->
    integer;
mysql_data_type(<<"tinyint", _/binary>>) ->
    integer;
mysql_data_type(<<"int", _/binary>>) ->
    integer;
mysql_data_type(<<"double", _/binary>>) ->
    integer;
mysql_data_type(<<"float", _/binary>>) ->
    integer;
mysql_data_type(<<"decimal", _/binary>>) ->
    integer;
mysql_data_type(<<"mediumint", _/binary>>) ->
    integer;
mysql_data_type(<<"char", _/binary>>) ->
    binary;
mysql_data_type(<<"varchar", _/binary>>) ->
    binary;
mysql_data_type(<<"tinytext", _/binary>>) ->
    binary;
mysql_data_type(<<"text", _/binary>>) ->
    binary;
mysql_data_type(<<"mediumtext", _/binary>>) ->
    binary;
mysql_data_type(<<"longtext", _/binary>>) ->
    binary;
mysql_data_type(_) ->
    other_type.

is_term(String) ->
    with_tokens(String, fun(Tokens) ->
                                parse_check(Tokens)
                        end).

with_tokens(String, ParseCheckFun) ->
    case erl_scan:string(String++".") of
        {ok, Tokens, _} ->
            ParseCheckFun(Tokens);
        _ ->
            false
    end.
parse_check(Tokens) ->
    case erl_parse:parse_term(Tokens) of
        {ok, Term}
          when is_integer(Term);
               is_tuple(Term);
               is_list(Term) -> 
            true;
        _ -> 
            false
    end.

%% 规则，参考自Emacs的erlang style
%% -record(data_base_activity, {	
%%           id,                                   %
%%           10个空格开始                             48个开始
%% 如果变量名超过48个空格，就是","后面加个空格作为%起始位置  
-define(COMMENT_START, 48).
-define(FIELD_START, 10).

%% FieldsInfo [{Field, Default, Comment}..]
%% 数据格式都是二进制，Default处理好字符串，二进制加双引号尖括号的问题
%% 这个函数专门拼接record的代码
generate_record_source(RecordName, FieldsInfo) ->
    CalLen = fun
                 (0, _) ->
                     0;
                 (Len1, Add) ->
                     Len1 + Add
             end,
    AddLenFieldsInfo = [{Field, Default, Comment, CalLen(byte_size(Field), 1) + CalLen(byte_size(Default), 3)} || 
                           {Field, Default, Comment} <- FieldsInfo],
    % io:format("~p~n", [AddLenFieldsInfo]),
    MaxFieldLen = field_max_len(AddLenFieldsInfo),
    CommentStart = max(?FIELD_START + MaxFieldLen + 1, ?COMMENT_START),
    % io:format("CommentStart ~p~n", [CommentStart]),
    RecordBeginStr = lists:concat(["-record(", RecordName, ", {\n"]),
    FieldsStr = generate_field_source(AddLenFieldsInfo, CommentStart),
    RecordEndStr = "         }).\n",
    [RecordBeginStr, FieldsStr, RecordEndStr].
  
field_max_len(FieldsInfo) ->
    lists:max([Len || {_, _, _, Len} <- FieldsInfo]).

exp_str(Field, <<>>) ->
    Field;
exp_str(Field, Default) ->
    [Field, " = ", Default].

generate_field_source([{Field, Default, Comment, Len}], CommentStart) ->
    SpaceBetweenFieldAndComment = 
        lists:duplicate((CommentStart - (?FIELD_START + Len - 1)), " "), %% 最后一个字段，少个","，一开始多算了
    FieldStartSpace = lists:duplicate(?FIELD_START, " "),
    FieldStr = [FieldStartSpace, 
                exp_str(Field, Default), 
                SpaceBetweenFieldAndComment, "%% ", Comment, "\n"],    
    [FieldStr];
generate_field_source([{Field, Default, Comment, Len}|FieldsInfo], CommentStart) ->
    SpaceBetweenFieldAndComment = 
        lists:duplicate((CommentStart - (?FIELD_START + Len)), " "),
    FieldStartSpace = lists:duplicate(?FIELD_START, " "),
    FieldStr = [FieldStartSpace, 
                exp_str(Field, Default), ",", 
                SpaceBetweenFieldAndComment, "%% ", Comment, "\n"],
    % io:format("~w~n", [FieldsInfo]),
    [FieldStr | generate_field_source(FieldsInfo, CommentStart)].

%% 生成table_record_info.erl

table_record_info(TableConfs, SrcPath) ->
    FileName = filename:join(SrcPath, "table_record_info.erl"),
    case check_new_version(table_record_info, TableConfs, FileName) of
        true ->            
            CodingStr = "%% coding: utf-8\n", %% for erl_tidy 识别我们代码的编码，否则格式化的话会选择其他编码，导致乱码
            WarningStr = <<"%% Warning:本文件由make_record自动生成，请不要手动修改\n"/utf8>>,
            ModuleStr = "-module(table_record_info).\n",
            ExportAllStr = "-export([table/1, record/1]).\n",

            TableSrcList = lists:map(fun({Table, Record}) ->
                                             lists:concat(["table(", Record, ")->", Table])
                                     end, TableConfs),
            RecordSrcList = lists:map(fun({Table, Record}) ->
                                             lists:concat(["record(", Table, ")->", Record])
                                      end, TableConfs),
            DefaultFun = fun(FunName) ->
                                 lists:concat([FunName, "(_) -> undefined.\n"])
                         end,
            TableSrc = string:join(TableSrcList ++ [DefaultFun(table)], ";\n"),
            RecordSrc = string:join(RecordSrcList ++ [DefaultFun(record)], ";\n"),

            Src = [CodingStr, WarningStr, ModuleStr, ExportAllStr, TableSrc, RecordSrc],
            
            file:write_file(FileName, [Src]),
            erl_tidy:file(FileName, [{backups, false}]),
            red_print("table_record_info generate ok~n", []);            
        false ->
            io:format("~s same~n", [table_record_info])
    end.


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

red_print(Format, Args) ->
    case os:type() of
        {unix,  _} -> 
            io:format("\e[1;31m" ++ Format ++ "\e[0;38m", Args);
        {win32, _} -> 
            io:format(Format, Args)
    end.

