-module(app_misc).

-export([start/1]).

-export([get_env/2, get_env/1]).
-export([ensure_application_loaded/0]).
-export([get_module/1, version/0]).
-export([init/0, mochi_set/2, mochi_get/1]).

-export([root_dir/0]).

-include("common.hrl").

start(App) ->
    start_ok(App, application:start(App)).

start_ok(_App, ok) -> ok;
start_ok(_App, {error, {already_started, _App}}) -> ok;
start_ok(App, {error, {not_started, Dep}}) ->
    ok = start(Dep),
    start(App);
start_ok(App, {error, Reason}) ->
    erlang:error({app_start_failed, App, Reason}).

init() ->
    mochi_set_value().

mochi_set_value() ->
    [mochi_set(Key, get_app_env(Key)) || Key <- [db_nodes] ],
    mochi_set(root_dir, root_dir()), %% cover编译后会丢失ebin路径信息，所以预先保存起来。
    ok.
%% 开服需要的信息就直接get_env
%% 如果常取的数据，比如server_id，那么可以通过mochiweb golbal动态编译，然后直接函数调用
get_app_env(App, Key, Def) ->
  case application:get_env(App, Key) of
      undefined -> 
          Def;
      {ok, Value} -> 
          Value
  end.

%%api
get_env(db_nodes) ->
    mochi_get(db_nodes);
get_env(Key) ->
  get_app_env(Key, undefined).

get_env(Key, Def) ->
    get_app_env(Key, Def).

get_app_env(Key) ->
    get_app_env(Key, undefined).
get_app_env(Key, Def) ->
  get_app_env(?APP_NAME, Key, Def).

ensure_application_loaded() ->
    case application:load(?APP_NAME) of
        ok ->
            ok;
        {error, {already_loaded, _}} ->
            ok
    end.

get_module(App) ->
    application:get_key(App, modules).

get_vsn(App) ->
    application:get_key(App, vsn).

version() ->
    {ok, Vsn} = get_vsn(?APP_NAME),
    Vsn.

mochi_set(Key, Value) ->
    mochiglobal:put(Key, Value).

mochi_get(Key) ->
    case mochiglobal:get(Key) of
        undefined ->
            error_logger:warning_msg("mochi_get error Key ~p~n", [Key]),
            get_app_env(Key);
        Value ->
            Value
    end.


%% 这个做法比较不合理，但是能工作。
%% code:lib_dir有局限性，就是app目录名和app名要一致
root_dir() ->
    case mochiglobal:get(root_dir) of
        undefined ->
            %% 兼容脚本启动的调用，这个时候可能并没有扔到mochigolbal
            remove_suffix_ebin(code:which(list_to_atom(atom_to_list(?APP_NAME) ++ "_app")));
        Value ->
            Value
    end.

remove_suffix_ebin(Path) ->
    filename:dirname(filename:dirname(Path)).
