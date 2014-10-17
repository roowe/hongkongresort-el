%% coding: utf-8
%% Warning:本文件由make_record自动生成，请不要手动修改
-module(table_record_info).

-export([record/1, table/1]).

table(base_error_list) -> base_error_list;
table(activity) -> activity;
table(comment) -> comment;
table(user) -> user;
table(login) -> login;
table(activity_image_relation) ->
    activity_image_relation;
table(image) -> image;
table(user_activity_relation) -> user_activity_relation;
table(notification) -> notification;
table(_) -> undefined.

record(base_error_list) -> base_error_list;
record(activity) -> activity;
record(comment) -> comment;
record(user) -> user;
record(login) -> login;
record(activity_image_relation) ->
    activity_image_relation;
record(image) -> image;
record(user_activity_relation) ->
    user_activity_relation;
record(notification) -> notification;
record(_) -> undefined.
