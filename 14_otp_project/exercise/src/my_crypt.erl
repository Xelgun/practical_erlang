%%%-------------------------------------------------------------------
%%% @author o_aduev
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Oct 2018 12:49 PM
%%%-------------------------------------------------------------------
-module(my_crypt).
-author("o_aduev").
-behavior(gen_server).

%% API
-export([init/1, handle_call/3, handle_cast/2, start_link/0, handle_info/2, terminate/2, code_change/3]).
-export([hash/1, get_key/0, set_key/1, encode/1]).
-export([get_key/1, encode/2]).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  InitialKey = application:get_env(my_crypt, crypt_key),
  {ok, #{key => InitialKey}}.

get_key() ->
  gen_server:call(?MODULE, get_key).

encode(Data) ->
  gen_server:call(?MODULE, {encode, Data}).

set_key(Key) ->
  gen_server:call(?MODULE, {set_key, Key}).

hash(Data) ->
  Ints = crypto:bytes_to_integer(Data),
  BinaryInts = integer_to_binary(Ints),
  BinaryInts.

handle_call({encode, Data}, _, State) ->
  {reply, encode(Data, State), State};


handle_call(get_key, _, State) ->
  {reply, get_key(State), State};


handle_call({set_key, Key}, _, State0) ->
  State1 = maps:put(key, Key, State0),
  {reply, ok, State1};

handle_call(_, _, State) ->
  {noreply, State}.

handle_cast(_, State) ->
  {noreply, State}.


handle_info(_, State) ->
  {noreply, State}.

encode(Data, State) ->
  Key = get_key(State),
  HashedKey =  hash(Key),
  HashedData =  hash(Data),
  Sum = binary_to_integer(HashedData) + binary_to_integer(HashedKey),
  integer_to_binary(Sum).

terminate(_, _State) ->
  ok.

code_change(_, State, _) ->
  {ok, State}.

get_key(State) ->
  maps:get(key, State).