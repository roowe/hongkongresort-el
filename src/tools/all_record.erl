-module(all_record).
-include("define_mysql.hrl").
-include("define_data_generate.hrl").
-include("db_base_error_list.hrl").
-include("db_activity.hrl").
-define(MATCH_SPEC(Record), #Record{_='_'}).
-export([get_fields/1]).
-export([is_record/1]).
-export([match_info/1]).
-export([new/1]).

is_record(activity) ->
    true;
is_record(base_error_list) ->
    true;
is_record(generate_conf) ->
    true;
is_record(record_mysql_info) ->
    true;
is_record(_) ->
 false.
get_fields(activity) ->
    record_info(fields, activity);
get_fields(base_error_list) ->
    record_info(fields, base_error_list);
get_fields(generate_conf) ->
    record_info(fields, generate_conf);
get_fields(record_mysql_info) ->
    record_info(fields, record_mysql_info);
get_fields(_) ->
    [].
match_info(activity) ->
    ?MATCH_SPEC(activity);
match_info(base_error_list) ->
    ?MATCH_SPEC(base_error_list);
match_info(generate_conf) ->
    ?MATCH_SPEC(generate_conf);
match_info(record_mysql_info) ->
    ?MATCH_SPEC(record_mysql_info);
match_info(Table) ->
    throw({match_info, not_match, Table}).
new(activity) ->
     io:format("~w~n", [lists:zip(record_info(fields, activity), tl(tuple_to_list(#activity{})))]);
new(base_error_list) ->
     io:format("~w~n", [lists:zip(record_info(fields, base_error_list), tl(tuple_to_list(#base_error_list{})))]);
new(generate_conf) ->
     io:format("~w~n", [lists:zip(record_info(fields, generate_conf), tl(tuple_to_list(#generate_conf{})))]);
new(record_mysql_info) ->
     io:format("~w~n", [lists:zip(record_info(fields, record_mysql_info), tl(tuple_to_list(#record_mysql_info{})))]);
new(_) ->
 undefined.

