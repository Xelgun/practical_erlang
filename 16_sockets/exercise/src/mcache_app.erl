-module(mcache_app).
-behaviour(application).

-export([start/0, start/2, stop/1]).


start() ->
    application:start(mcache),
    ok.

start(_, _) ->
    mcache_sup:start_link().

stop(_) ->
    ok.
