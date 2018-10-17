%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Oct 2018 2:55 PM
%%%-------------------------------------------------------------------
-module(mcache_server).
-behavior(gen_server).

%% API
-compile(export_all).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, listen/2, send/1, mapping/1]).
-export([parse_protocol/1, key_value/2]).

-define(get(Key), {get, Key}).
-define(gets(Keys), {gets, Keys}).
-define(set(Key, Value), {set, Key, Value}).
-define(add(Key, Value), {add, Key, Value}).
-define(replace(Key, Value), {replace, Key, Value}).
-define(append(Key, Value), {append, Key, Value}).
-define(prepend(Key, Value), {prepend, Key, Value}).
-define(delete(Key), {delete, Key}).
-define(unknown, unknown).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init([]) ->
  {ok, Port} = application:get_env(mcache, port),
  {ok, PoolSize} = application:get_env(mcache, accept_pool_size),
  {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {active, false}, {packet, line}, {reuseaddr, true}]),
  [spawn(?MODULE, listen, [Id, ListenSocket]) || Id <- lists:seq(1, PoolSize)],
  {ok, ListenSocket}.



listen(Id, ListenSocket) ->
  case gen_tcp:accept(ListenSocket) of
    {ok, Socket} ->
      handle_connection(Id, ListenSocket, Socket);
    _ -> io:format("Fail~n")
  end.

handle_connection(Id, ListenSocket, Socket) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, Msg0} ->
      io:format("Msg0: ~p~n", [Msg0]),
      Msg1 = binary:part(Msg0, 0, byte_size(Msg0) - 2),
      Reply = send(mapping(Msg1)),
      gen_tcp:send(Socket, <<Reply/binary, "\r\n">>),
      handle_connection(Id, ListenSocket, Socket);
    {error, closed} ->
      listen(Id, ListenSocket)
  end.

mapping(Msg) ->
  [Action | Rest0] = binary:split(Msg, <<" ">>),
  io:format("Msg: ~p Action: ~p Rest0: ~p ~n", [Msg, Action, Rest0]),
  [Key | Value] =
  case Rest0 of
    [] -> [<<>>, <<>>];
    [Rest1] -> binary:split(Rest1, <<" ">>)
  end,
  io:format("Key: ~p Value: ~p~n", [Key, Value]),
  case {Action, Key, Value} of
    {<<"SET">>, Key, Value} -> ?set(Key, Value);
    {<<"GET">>, Key, []} -> ?get(Key);
    {<<"GETS">>, Key, Values0} ->
                                 io:format("Key: ~p Values0: ~p ~n", [Key, Values0]),
                                 [Values1] = Values0,
                                 io:format("Values1: ~p ~n", [Values1]),
                                 Values2 = binary:split(Values1, <<" ">>, [global]),
                                 io:format("Values2: ~p ~n", [Values2]),
                                 Values3 = [Key] ++ Values2,
                                 io:format("Values3: ~p ~n", [Values3]),
                                 ?gets(Values3);
    {<<"ADD">>, Key, Value} -> ?add(Key, Value);
    {<<"REPLACE">>, Key, Value} -> ?replace(Key, Value);
    {<<"APPEND">>, Key, Value0} ->
                                io:format("Value0: ~p~n", [Value0]),
                                [Value1] = Value0,
                                io:format("Value1: ~p~n", [Value1]),
                                ?append(Key, Value1);
    {<<"PREPEND">>, Key, Value0} ->
                                io:format("Value0: ~p~n", [Value0]),
                                [Value1] = Value0,
                                 io:format("Value1: ~p~n", [Value1]),
                                ?prepend(Key, Value1);
    {<<"DELETE">>, Key, []}->  ?delete(Key);
    Unkwnown ->
      io:format("Unkwnown: ~p", [Unkwnown]),
      ?unknown
  end.


send(?set(Key, Value)) ->
  mcache:set(Key, Value),
  <<"STORED">>;

send(?get(Key)) ->
  case mcache:get(Key) of
    {ok, [Value]} -> <<"VALUE ", Key/binary, " ", Value/binary, "\r\nEND">>;
    {ok, Value} -> <<"VALUE ", Key/binary, " ", Value/binary, "\r\nEND">>;
    {error, not_found} -> <<"NOT FOUND">>
  end;

send(?gets(Keys)) ->
  {[], Res} = lists:foldl(
    fun(R, {[Key | Rest], Acc}) ->
      Value = case R of
                {ok, [V]} -> V;
                {ok, V} -> V;
                {error, not_found} -> <<"NOT FOUND">>
              end,
      io:format("Rest: ~p Acc: ~p Key: ~p Value: ~p ~n", [Rest, Acc, Key, Value]),
      {Rest, <<Acc/binary, "VALUE ", Key/binary, " ", Value/binary, "\r\n">>}
    end,
    {Keys, <<>>},
    mcache:gets(Keys)),
  <<Res/binary, "END">>;

send(?add(Key, Value)) ->
  case mcache:add(Key, Value) of
    ok -> <<"STORED">>;
    _ -> <<"EXISTS">>
  end;

send(?replace(Key, Value)) ->
  case mcache:replace(Key, Value) of
    ok -> <<"STORED">>;
    _ -> <<"NOT FOUND">>
  end;

send(?append(Key, Value)) ->
  case mcache:append(Key, Value) of
    ok -> <<"STORED">>;
    _ -> <<"NOT FOUND">>
  end;

send(?prepend(Key, Value)) ->
  case mcache:prepend(Key, Value) of
    ok -> <<"STORED">>;
    _ -> <<"NOT FOUND">>
  end;

send(?delete(Key)) ->
  case mcache:delete(Key) of
    ok -> <<"DELETED">>;
    _ -> <<"NOT FOUND">>
  end;

send(?unknown) ->
  <<"UNKNOWN REQUEST">>.


handle_call(_Request, _From, State) ->
  {noreply, State}.

handle_cast(_, State) ->
  {noreply, State}.


handle_info(_, State) ->
  {noreply, State}.


terminate(_, _State) ->
  ok.


code_change(_, State, _) ->
  {ok, State}.