-module(sup_2).

-export([start_link/0, init/1, add_worker/1, remove_worker/1]).

start_link() ->
  supervisor:start_link(?MODULE, []).

init(_Arg0) ->
  SupervisorSpecification = #{strategy => one_for_one,
    intensity => 10,
    period => 1000},
  Worker3 = genereate_spec(worker_3),
  Worker4 = genereate_spec(worker_4),
  {ok, {SupervisorSpecification, [Worker3, Worker4]}}.


add_worker(Pid) ->
  supervisor:start_child(?MODULE, genereate_spec(Pid)).

remove_worker(Pid) ->
  supervisor:terminate_child(?MODULE, Pid),
  supervisor:delete_child(?MODULE, Pid).

genereate_spec(Pid) ->
  #{id => Pid,
    start => {worker, start_link, [Pid]},
    restart => permanent,
    shutdown => 2000,
    type => worker,
    modules => [worker]
  }.
