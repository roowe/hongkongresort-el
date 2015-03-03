-module(lib_notification).

-export([insert_and_push/2]).

-include("define_info_0.hrl").
-include("common.hrl").
-include("db_notification.hrl").

insert_and_push(#notification{
                   from = Same,
                   to = Same
                  }, _) ->
    ingore;
insert_and_push(Notification0, DataFun) ->
    ?DEBUG("Notification ~p~n", [Notification0]),
    case db_notification:insert(Notification0#notification{
                                  generated_time = time_misc:long_unixtime(),
                                  content = data_base_notification:zh_hk(Notification0)
                                 }) of
        {ok, Notification} ->
            lib_user:incr_unread_count(Notification#notification.to),
            Msg = ?JSON([{cmd, Notification#notification.cmd}, {data, DataFun(Notification)}]),
            ws_controller:publish(Notification#notification.to, Msg);
        {error, _Error} ->
            {fail, ?INFO_DB_ERROR}
    end.

