-module(worker).
-behavior(gen_server).

-export([start_link/1, ping/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(ping, ping).


start_link(Pid) ->
  gen_server:start_link(?MODULE, [Pid], []).

ping(Pid) ->
  gen_server:call(Pid, ?ping).

init(Pid) ->
  {ok, Pid}.

handle_call(?ping, _, Pid) ->
  {reply, {Pid, self()}, Pid};

handle_call(_, _, Pid) ->
  {noreply, Pid}.

handle_cast(_, Pid) ->
  {noreply, Pid}.

handle_info(_, Pid) ->
  {noreply, Pid}.

terminate(_, _) ->
  ok.

code_change(_, Pid, _) ->
  {ok, Pid}.