-module(notification_controller).

-export([init/2]).
-export([terminate/3]).

-export([execute_post/3]).

-include("db_activity.hrl").
-include("db_user.hrl").
-include("db_notification.hrl").

-include("common.hrl").
-include("define_info_0.hrl").
-define(ACTION_MARK, [<<"mark">>]).

-define(PARAM_ID, <<"id">>).
-define(PARAM_IS_READ, <<"is_read">>). 
-define(PARAM_TOKEN, <<"token">>). 
     
init(Req, _Opts) ->
    PostParameter = [{?ACTION_MARK, [{?PARAM_ID, int, required},
                                     {?PARAM_IS_READ, int, required},
                                     {?PARAM_TOKEN, binary, optional}]
                     }],
    ControllerOpts = [{post_parameter, PostParameter}],
	{controller_helper, Req, ControllerOpts}.

%%  /el/notification/mark
%% id=2&token=0a029a1451b987fd3401f3820ec5139a&is_read=1
execute_post(?ACTION_MARK, [Id, IsRead, Token], _Req) ->   
    case lib_user:user_id_by_token(Token) of
        {fail, Reason} ->
            {fail, Reason};
        {ok, UserId} ->
            case db_notification:find(Id) of
                {ok, [#notification{
                         to = UserId
                        } = Notification]} ->
                    lib_user:dec_unread_count(UserId),
                    db_notification:update(Notification#notification{
                                             is_read = IsRead
                                            }),
                    {fail, ?INFO_OK};
                {error, _} ->
                    {fail, ?INFO_DB_ERROR};
                {ok, _} ->
                    {fail, ?INFO_PARAMETER_ERROR}
            end
    end.
           
terminate(_Reason, _Req, _State) ->
	ok.

