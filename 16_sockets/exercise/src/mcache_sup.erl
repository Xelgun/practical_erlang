%%%-------------------------------------------------------------------
%%% @author o_aduev
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Oct 2018 2:33 PM
%%%-------------------------------------------------------------------
-module(mcache_sup).

-behavior(supervisor).

%% API
-export([init/1, start_link/0]).

-compile(export_all).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init(_Args) ->
  SupervisorSpecification =
    #{strategy => one_for_one,
      intensity => 10,
      period => 1000
    },

  ChildsSpecification =
    [
      #{id => mcache,
        start => {mcache, start_link, []},
        restart => permanent,
        shutdown => 2000,
        type => worker,
        modules => [mcache]
      },
      #{id => mcache_server,
        start => {mcache_server, start_link, []},
        restart => permanent,
        shutdown => 2000,
        type => worker,
        modules => [mcache_server]
      }
    ],
  {ok, {SupervisorSpecification, ChildsSpecification}}.
