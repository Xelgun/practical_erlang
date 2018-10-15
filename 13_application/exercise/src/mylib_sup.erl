-module(mylib_sup).

-behaviour(supervisor).


-export([init/1, start_link/0]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
  SupervisorSpecification = #{strategy => one_for_one,
    intensity => 10,
    period => 1000},
  MyLibWorker =  #{id => mylib_worker,
    start => {mylib_worker, start_link, []},
    restart => permanent,
    shutdown => 2000,
    type => worker,
    modules => [mylib_worker]
  },
  {ok, {SupervisorSpecification, [MyLibWorker]}}.

