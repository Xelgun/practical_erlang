%%%-------------------------------------------------------------------
%%% @author o_aduev
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Oct 2018 12:48 PM
%%%-------------------------------------------------------------------
-module(my_crypt_sup).
-author("o_aduev").
-behavior(supervisor).

%% API
-export([init/1, start_link/0]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
  SupervisorSpecification = #{strategy => one_for_one,
    intensity => 10,
    period => 1000},
  MyCrypt =  #{id => my_crypt,
    start => {my_crypt, start_link, []},
    restart => permanent,
    shutdown => 2000,
    type => worker,
    modules => [my_crypt]
  },
  {ok, {SupervisorSpecification, [MyCrypt]}}.
