-module(mod_activity_noti).

-behaviour(gen_server).

%% API
-export([start_link/0]).

-export([begin_noti/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("common.hrl").
-include("db_notification.hrl").

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
begin_noti(ActivityId, BeginTimestamp) ->
    Now = time_misc:long_unixtime(),
    %% ?DEBUG("Now ~p, BeginTimestamp ~p~n", [Now, BeginTimestamp]),
    if 
        Now < BeginTimestamp ->
            %% ?DEBUG("ActivityId ~p, TimeDiff ~p~n" ,[ActivityId, BeginTimestamp-Now]),
            erlang:send_after(BeginTimestamp-Now, ?SERVER, {begin_noti, ActivityId});
        true ->
            ingore
    end.
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    gen_server:cast(self(), init_add_not_begin_accept_activity),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    ?WARNING_MSG("unknow request: ~p~n", [_Request]),
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(init_add_not_begin_accept_activity, State) ->
    case db_activity:not_begin_accept_activity_info() of
        {ok, ActivityInfos} ->
            [begin_noti(ActivityId, BeginTimestamp) || [ActivityId, BeginTimestamp] <- ActivityInfos];
        {error, Error} ->
            ?WARNING_MSG("DB Error ~p~n", [Error])
    end,
    {noreply, State};
handle_cast(_Msg, State) ->
    ?WARNING_MSG("unknow cast: ~p~n", [_Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({begin_noti, ActivityId}, State) ->
    ?DEBUG("running begin_noti ~p~n", [ActivityId]),
    case db_user_activity_relation:sign_up_user_ids(ActivityId) of
        {ok, UserIds} ->
            [lib_notification:insert_and_push(notification(ActivityId, UserId),
                                              fun notification_pack/1) || [UserId] <- UserIds],
            ok;
        {error, Error} ->
            ?DEBUG("DB Error ~p~n", [Error])
    end,
    {noreply, State};
handle_info(_Info, State) ->
    ?WARNING_MSG("unknow info: ~p~n", [_Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
notification(ActivityId, UserId) ->
    #notification{
       cmd = ?S2C_ACTIVITY_BEGIN,
       activity_id = ActivityId,
       content = <<"你報名的活動 id = <"/utf8,
                   (integer_to_binary(ActivityId))/binary,
                   ">已經開始啦， 活動後記得回來評價其他參與者哦！"/utf8>>,
       to = UserId
      }.

notification_pack(Notification) ->
    ?JSON([{id, Notification#notification.id},
           {activity_id, Notification#notification.activity_id},
           {content, Notification#notification.content}]).
