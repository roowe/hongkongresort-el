%% 该模块主要参考了boss_mail的封装，去掉了没用到的功能
%% https://github.com/ChicagoBoss/ChicagoBoss/blob/master/src/boss/boss_mail.erl
-module(mail_misc).

-export([send/2, send/3]).

-include("common.hrl").

-define(MAIL_ACCOUNT, "moyou_notify@163.com").

-define(MAIL_OPTIONS, [{relay, "smtp.163.com"}, 
                       {username, ?MAIL_ACCOUNT}, 
                       {password, "moyou@1234"},
                       {ssl, true}, 
                       {tls, true}, 
                       {auth, always}]).

-define(TO_ADDRESS_LIST, ["bestluoliwe@gmail.com", 
                          "15913149160@139.com"]).
%% moyou_notify@163.com
%% * 问题一：
%% 您大学辅导员的名字是？
%% 答案：erlang
%% * 问题二：
%% 您孩子的名字是？
%% 答案：opensource

%% * 问题三：
%% 对你影响最大的人是？
%% 答案：cplusplus
%% test() ->
%%     do_send(?MAIL_ACCOUNT, "bestluoliwe@gmail.com", hmisc:rand_str(), "猜猜我是谁").
    
send(Subject, Body) ->
    [do_send(?MAIL_ACCOUNT, ToAddress, Subject, Body) || ToAddress <- ?TO_ADDRESS_LIST].

send(Subject, Body, BodyArgs) ->
    FormatBody = io_lib:format(Body, BodyArgs),
    send(Subject, FormatBody).

do_send(FromAddress, ToAddress, Subject, Body) ->
    MessageHeader = build_message_header([{"Subject", Subject},
                                          {"To", ToAddress},
                                          {"From", FromAddress}],
                                         "text/plain"), 
    deliver(?MAIL_OPTIONS, FromAddress, ToAddress, 
            fun() -> 
                    [MessageHeader, "\r\n", convert_unix_newlines_to_dos(Body)] 
            end,
            fun
                ({exit, Error}) ->             
                   ?WARNING_MSG("send_mail exit ~p~n", [Error]);
                ({error, Type, Message}) ->
                   ?WARNING_MSG("send_mail error, Type ~p, Message ~p~n", [Type, Message]);
                ({ok, Receipt}) ->
                   ?DEBUG("send_mail successful, result is ~p~n", [Receipt])
           end),
    ok.

deliver(Options, FromAddress, ToAddress, BodyFun, ResultFun) ->
    Email = {FromAddress, [ToAddress], BodyFun},
    gen_smtp_client:send(Email, Options, ResultFun).




convert_unix_newlines_to_dos(Body) when is_binary(Body) ->
    convert_unix_newlines_to_dos(binary_to_list(Body));
convert_unix_newlines_to_dos(Body) when is_list(Body) ->
    convert_unix_newlines_to_dos(Body, []).

convert_unix_newlines_to_dos([], Acc) ->
    lists:reverse(Acc);
convert_unix_newlines_to_dos([$\r, $\n|Rest], Acc) ->
    convert_unix_newlines_to_dos(Rest, [$\n, $\r|Acc]);
convert_unix_newlines_to_dos([$\n|Rest], Acc) ->
    convert_unix_newlines_to_dos(Rest, [$\n, $\r|Acc]);
convert_unix_newlines_to_dos([H|T], Acc) when is_binary(H); is_list(H) ->
    convert_unix_newlines_to_dos(T, [convert_unix_newlines_to_dos(H)|Acc]);
convert_unix_newlines_to_dos([H|T], Acc) ->
    convert_unix_newlines_to_dos(T, [H|Acc]).

build_message_header(HeaderFields, DefaultMimeType) ->
    ContentType = proplists:get_value("Content-Type", HeaderFields, DefaultMimeType),
    AllHeaders = [{"Content-Type", ContentType} | HeaderFields],
    add_fields(AllHeaders, [], []).

add_fields([], _, Acc) ->
    lists:reverse(Acc);
add_fields([{Key, Value}|Rest], Seen, Acc) ->
    case proplists:get_value(Key, Seen) of
        undefined ->
            add_fields(Rest, [Key|Seen], [[Key, ": ", Value, "\r\n"] | Acc]);
        _ ->
            add_fields(Rest, Seen, Acc)
    end.


