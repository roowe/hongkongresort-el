-module(generate_all_record).

-export([start/0]).

start() ->
    HeaderFiles = header_files(),
    ModuleNameStr = "-module(all_record).\n",    
    ExportStr = "-export([get_fields/1]).\n-export([is_record/1]).\n-export([match_info/1]).\n-export([new/1]).\n\n",
    DefStr = "-define(MATCH_SPEC(Record), #Record{_='_'}).\n",
    {ok, MatchPattern} = re:compile("\n-record\\(([a-z_]*),", []),
    {IncludFiles, AllRecord} = 
        lists:foldl(fun(HeaderFile, {AccIncludFiles, AccRecords}=Acc) ->
                            case get_records_from_file(HeaderFile, MatchPattern) of
                                [] ->
                                    Acc;
                                Records ->
                                    {[HeaderFile|AccIncludFiles], Records ++ AccRecords}
                            end
                    end, {[], []}, HeaderFiles),
    IncludeStrs = [include_str(HeaderFile) || HeaderFile <- IncludFiles],
    UniqRecords = lists:usort(AllRecord),
    GetFieldsFunStr = [get_fields_str(RecordName) || RecordName <- UniqRecords] ++ "get_fields(_) ->\n    [].\n",
    %AllRecordStr = all_record_str(UniqRecords),
    IsRecords = [is_record_str(RecordName) || RecordName <- UniqRecords] ++ "is_record(_) ->\n false.\n",
    MatchInfoStrs = [one_match_info(Record) || Record <- UniqRecords] ++ ["match_info(Table) ->
    throw({match_info, not_match, Table}).
"],
    News = [new_str(RecordName) || RecordName <- UniqRecords] ++ "new(_) ->\n undefined.\n",
    file:write_file(filename:join([app_misc:root_dir(), "src/tools/all_record.erl"]), io_lib:format("~s~n", [ModuleNameStr ++ IncludeStrs ++ DefStr ++ ExportStr ++ IsRecords ++ GetFieldsFunStr ++ MatchInfoStrs ++ News])).


get_records_from_file(File, MatchPattern) ->
    {ok, FileContent} = file:read_file(File),
    case re:run(FileContent, MatchPattern, [global, {capture, [1], list}]) of
        {match, RecordNames} ->
            lists:append(RecordNames);
        nomatch ->
            []
    end.
include_str(HeaderFile) ->
    "-include(\"" ++ filename:basename(HeaderFile) ++ "\").\n".

get_fields_str(RecordName) ->
    "get_fields(" ++ RecordName ++ ") ->\n    record_info(fields, " ++ RecordName ++ ");\n".


%% all_record_str(Records) ->
%%     "all_records() ->\n" ++ "    [" ++ string:join(Records, ", ") ++ "].\n\n".      

is_record_str(Record) ->
    "is_record(" ++ Record ++ ") ->\n    true;\n".

one_match_info(Record) ->
    lists:concat(["match_info(", Record, ") ->\n    ?MATCH_SPEC(", Record, ");\n"]).

new_str(Record) ->
%% new(player) ->    
%%     io:format("~w~n", [lists:zip(record_info(fields, player), tl(tuple_to_list(#player{})))]);
    lists:concat(["new(", Record, ") ->\n     io:format(\"~w~n\", [lists:zip(record_info(fields, ", Record, "), tl(tuple_to_list(#", Record, "{})))]);\n"]).


header_files() ->
    %["/home/roowe/happytree/server_p02/include/define_player.hrl", "/home/roowe/happytree/server_p02/include/db_base_activity.hrl"].
    filelib:wildcard(filename:join([app_misc:root_dir(), "include/*.hrl"])).
