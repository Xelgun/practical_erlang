-module(mylib_worker).
-behavior(gen_server).

-export([start_link/0, get_version/0, get_modules/0, get_min_val/0, get_connection_timeout/0, all_apps/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


init([]) ->
  {ok, no_state}.

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


get_version() ->
  {ok, Version} =  application:get_key(mylib, vsn),
  Version.


get_modules() ->
  {ok, Modules} =  application:get_key(mylib, modules),
  Modules.

get_min_val() ->
  {ok, MinVal} =  application:get_env(mylib, min_val),
  MinVal.

get_connection_timeout() ->
  {ok, ConnectionTimeout} =  application:get_env(mylib, connection_timeout),
  ConnectionTimeout.

all_apps() ->
  AppsList = lists:foldl(
    fun({App, Desc, Version}, Acc) ->
      [{App, #{description => Desc, version => Version}} | Acc]
    end,
    [],
    application:which_applications()
  ),
  maps:from_list(AppsList).


handle_call(_Request, _From, State) ->
  {noreply, State}.



handle_cast(_, State) ->
  {noreply, State}.


handle_info(_, State) ->
  {noreply, State}.


code_change(_, State, _) ->
  {ok, State}.

terminate(_, _) ->
  ok.
