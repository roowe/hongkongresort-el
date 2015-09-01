-module(all_record).
-include("define_mysql.hrl").
-include("define_data_generate.hrl").
-include("db_user_activity_relation.hrl").
-include("db_user.hrl").
-include("db_notification.hrl").
-include("db_login.hrl").
-include("db_image.hrl").
-include("db_comment.hrl").
-include("db_base_error_code.hrl").
-include("db_activity_image_relation.hrl").
-include("db_activity.hrl").
-define(MATCH_SPEC(Record), #Record{_='_'}).
-export([get_fields/1]).
-export([is_record/1]).
-export([match_info/1]).
-export([new/1]).

is_record(activity) ->
    true;
is_record(activity_image_relation) ->
    true;
is_record(base_error_code) ->
    true;
is_record(comment) ->
    true;
is_record(generate_conf) ->
    true;
is_record(image) ->
    true;
is_record(login) ->
    true;
is_record(notification) ->
    true;
is_record(player) ->
    true;
is_record(record_mysql_info) ->
    true;
is_record(user_activity_relation) ->
    true;
is_record(_) ->
 false.
get_fields(activity) ->
    record_info(fields, activity);
get_fields(activity_image_relation) ->
    record_info(fields, activity_image_relation);
get_fields(base_error_code) ->
    record_info(fields, base_error_code);
get_fields(comment) ->
    record_info(fields, comment);
get_fields(generate_conf) ->
    record_info(fields, generate_conf);
get_fields(image) ->
    record_info(fields, image);
get_fields(login) ->
    record_info(fields, login);
get_fields(notification) ->
    record_info(fields, notification);
get_fields(player) ->
    record_info(fields, player);
get_fields(record_mysql_info) ->
    record_info(fields, record_mysql_info);
get_fields(user_activity_relation) ->
    record_info(fields, user_activity_relation);
get_fields(_) ->
    [].
match_info(activity) ->
    ?MATCH_SPEC(activity);
match_info(activity_image_relation) ->
    ?MATCH_SPEC(activity_image_relation);
match_info(base_error_code) ->
    ?MATCH_SPEC(base_error_code);
match_info(comment) ->
    ?MATCH_SPEC(comment);
match_info(generate_conf) ->
    ?MATCH_SPEC(generate_conf);
match_info(image) ->
    ?MATCH_SPEC(image);
match_info(login) ->
    ?MATCH_SPEC(login);
match_info(notification) ->
    ?MATCH_SPEC(notification);
match_info(player) ->
    ?MATCH_SPEC(player);
match_info(record_mysql_info) ->
    ?MATCH_SPEC(record_mysql_info);
match_info(user_activity_relation) ->
    ?MATCH_SPEC(user_activity_relation);
match_info(Table) ->
    throw({match_info, not_match, Table}).
new(activity) ->
     io:format("~w~n", [lists:zip(record_info(fields, activity), tl(tuple_to_list(#activity{})))]);
new(activity_image_relation) ->
     io:format("~w~n", [lists:zip(record_info(fields, activity_image_relation), tl(tuple_to_list(#activity_image_relation{})))]);
new(base_error_code) ->
     io:format("~w~n", [lists:zip(record_info(fields, base_error_code), tl(tuple_to_list(#base_error_code{})))]);
new(comment) ->
     io:format("~w~n", [lists:zip(record_info(fields, comment), tl(tuple_to_list(#comment{})))]);
new(generate_conf) ->
     io:format("~w~n", [lists:zip(record_info(fields, generate_conf), tl(tuple_to_list(#generate_conf{})))]);
new(image) ->
     io:format("~w~n", [lists:zip(record_info(fields, image), tl(tuple_to_list(#image{})))]);
new(login) ->
     io:format("~w~n", [lists:zip(record_info(fields, login), tl(tuple_to_list(#login{})))]);
new(notification) ->
     io:format("~w~n", [lists:zip(record_info(fields, notification), tl(tuple_to_list(#notification{})))]);
new(player) ->
     io:format("~w~n", [lists:zip(record_info(fields, player), tl(tuple_to_list(#player{})))]);
new(record_mysql_info) ->
     io:format("~w~n", [lists:zip(record_info(fields, record_mysql_info), tl(tuple_to_list(#record_mysql_info{})))]);
new(user_activity_relation) ->
     io:format("~w~n", [lists:zip(record_info(fields, user_activity_relation), tl(tuple_to_list(#user_activity_relation{})))]);
new(_) ->
 undefined.

