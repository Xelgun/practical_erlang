%%%-------------------------------------------------------------------
%%% @author o_aduev
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Oct 2018 12:45 PM
%%%-------------------------------------------------------------------
-module(my_crypt_app).
-author("o_aduev").
-behavior(application).

%% API
-export([start/0, start/2, stop/1]).

start() ->
  application:start(my_crypt),
  ok.


start(_StartType, _StartArgs) ->
  my_crypt_sup:start_link().

stop(_State) ->
  ok.