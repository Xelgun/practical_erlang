%%%-------------------------------------------------------------------
%%% @author o_aduev
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Oct 2018 2:33 PM
%%%-------------------------------------------------------------------
-module(mcache).




-behavior(gen_server).

-export([init/1, handle_call/3, handle_cast/2, start_link/0, handle_info/2, code_change/3, terminate/2]).

-export([get/1, gets/1, add/2, set/2, replace/2, append/2,prepend/2, delete/1]).

%%-compile(export_all).

-define(get(Key), {get, Key}).
-define(gets(Keys), {gets, Keys}).
-define(set(Key, Value), {set, Key, Value}).
-define(add(Key, Value), {add, Key, Value}).
-define(replace(Key, Value), {replace, Key, Value}).
-define(append(Key, Value), {append, Key, Value}).
-define(prepend(Key, Value), {prepend, Key, Value}).
-define(delete(Key), {delete, Key}).

-record(state, {
  table :: ets:tid()
}).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  Table = ets:new(?MODULE, [set, protected, named_table]),
  {ok, #state{table = Table}}.


set(Key, Value) ->
  gen_server:call(?MODULE, ?set(Key, Value)).

get(Key) ->
  gen_server:call(?MODULE, ?get(Key)).

gets(Keys) ->
  lists:map(fun get/1, Keys).

delete(Key) ->
  case ?MODULE:get(Key) of
    {ok, _} -> gen_server:call(?MODULE, ?delete(Key));
    {error, not_found} -> {error, not_found}
  end.

add(Key, Value) ->
  case ?MODULE:get(Key) of
    {ok, _} -> {error, exists};
    {error, not_found} -> set(Key, Value)
  end.


replace(Key, Value) ->
  case ?MODULE:get(Key) of
    {ok, _} -> set(Key, Value);
    {error, not_found} -> {error, not_found}
  end.


append(Key, Value) ->
  io:format("Key: ~p Value: ~p~n", [Key, Value]),
  case ?MODULE:get(Key) of
    {ok, [OldValue]} ->
      io:format("Key: ~p Value: ~p OldValue: ~p~n", [Key, Value, OldValue]),
      set(Key, <<OldValue/binary, Value/binary>>);
    {ok, OldValue} ->
      io:format("Key: ~p Value: ~p OldValue: ~p~n", [Key, Value, OldValue]),
      set(Key, <<OldValue/binary, Value/binary>>);
    {error, not_found} -> {error, not_found};
    Unknown ->
              io:format("Unknown: ~p ~n", [Unknown]),
              {error, not_found}
  end.

prepend(Key, Value) ->
  case ?MODULE:get(Key) of
    {ok, [OldValue]} -> set(Key, <<Value/binary, OldValue/binary>>);
    {ok, OldValue} -> set(Key, <<Value/binary, OldValue/binary>>);
    {error, not_found} -> {error, not_found}
  end.

handle_call(?get(Key), _From,  State) ->
  case ets:lookup(?MODULE, Key) of
    [] -> {reply, {error, not_found}, State};
    [{Key, Val}] ->  {reply, {ok, Val}, State}
  end;

handle_call(?set(Key, Value), _From,  #state{table = Table} = State) ->
  ets:insert(Table, {Key, Value}),
  {reply, ok, State};

handle_call(?delete(Key), _From,   #state{table = Table} = State) ->
  ets:delete(Table, Key),
  {reply, ok, State};


handle_call(Request, _From, State) ->
  lager:error("unknown Request: ~p in ~p ~n", [Request, ?MODULE]),
  {noreply, State}.


handle_cast(_, State) ->
  {noreply, State}.


handle_info(_, State) ->
  {noreply, State}.


terminate(_, _State) ->
  ok.


code_change(_, State, _) ->
  {ok, State}.