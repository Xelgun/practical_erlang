-module(chat_room_manager).

-compile(export_all).


start() ->
  spawn(?MODULE, loop, [{#{}, #{}, #{}}]).



create_room(Server, RoomName) ->
  call(Server, {create_room, RoomName}).

remove_room(Server, RoomId) ->
  call(Server, {remove_room, RoomId}).


get_rooms(Server) ->
  call(Server, get_rooms).

add_user(Server, RoomId, UserName) ->
  call(Server, {add_user, RoomId, UserName}).


remove_user(Server, RoomId, UserName) ->
  call(Server, {remove_user, RoomId, UserName}).


get_users_list(Server, RoomId) ->
  call(Server, {get_users_list, RoomId}).

send_message(Server, RoomId, UserName, Message) ->
  call(Server, {send_message, RoomId, UserName, Message}).

get_messages_history(Server, RoomId) ->
  call(Server, {get_messages_history, RoomId}).

handle_call({create_room, RoomName}, {Rooms0, Users, Messages} =  State) ->
  case maps:size(Rooms0) of
    RoomSize when RoomSize >= 5 -> {{error, room_limit}, State};
    _ -> RoomId = uuid(),
      Rooms1 = maps:put(RoomId, RoomName, Rooms0),
      {{ok, RoomId}, {Rooms1, Users, Messages}}
  end;

handle_call({remove_room, RoomId}, {Rooms0, Users0, Messages} =  State) ->
  case maps:take(RoomId, Rooms0) of
    error -> {{error, room_not_found}, State};
    {_RoomName, Rooms1} -> Users1 = maps:filter(fun(_K,V) -> V =/= RoomId end, Users0),
                          {ok, {Rooms1, Users1, Messages}}
  end;


handle_call(get_rooms, {Rooms, _, _} = State) ->
%%  io:format(" Rooms: ~p, State: ~p~n", [Rooms, State]),
  {maps:to_list(Rooms), State};

handle_call({add_user, RoomId, UserName}, {Rooms, Users0, Messages} = State) ->
  case {maps:is_key(RoomId, Rooms), maps:is_key(UserName, Users0)} of
    {true, false} -> Users1 = maps:put(UserName, RoomId, Users0),
                     {ok, {Rooms, Users1, Messages}};
    {true, true} -> {{error, user_is_in_room}, State};
    {false, false} -> {{error, room_not_found}, State};
    {false, true} -> {{error, room_not_found}, State}
  end;

handle_call({remove_user, RoomId, UserName}, {Rooms, Users0, Messages} = State) ->
  case {maps:is_key(RoomId, Rooms), maps:is_key(UserName, Users0) andalso maps:get(UserName, Users0) =:= RoomId} of
    {true, false} -> {{error, user_not_in_room}, State};

    {true, true} -> Users1 = maps:without([UserName], Users0),
                    {ok, {Rooms, Users1, Messages}};
    {false, false} -> {{error, room_not_found}, State};
    {false, true} -> {{error, room_not_found}, State}
  end;

handle_call({get_users_list, RoomId}, {Rooms, Users, _} = State) ->
  case maps:is_key(RoomId, Rooms) of
    false -> {{error, room_not_found}, State};
    true -> UsersInRoom = maps:filter(fun(_K,V) -> V =:= RoomId end, Users),
            {{ok, lists:reverse(maps:keys(UsersInRoom))}, State}
  end;

handle_call({send_message, RoomId, UserName, Message}, {Rooms, Users, Messages0} = State) ->
  case {maps:is_key(RoomId, Rooms), maps:is_key(UserName, Users)} of
    {true, true} -> RoomMessageHistory = get_room_message_history(RoomId, Messages0),
                    Messages1 = maps:put(RoomId, [{UserName, Message} | RoomMessageHistory], Messages0),
                    {ok, {Rooms, Users, Messages1}};
    {true, false} -> {{error, user_not_in_room}, State};
    {false, false} -> {{error, room_not_found}, State};
    {false, true} -> {{error, room_not_found}, State}
  end;

handle_call({get_messages_history, RoomId}, {Rooms, _, Messages} = State) ->
  case maps:is_key(RoomId, Rooms) of
    true -> {{ok, get_room_message_history(RoomId, Messages)}, State};
    false -> {{error, room_not_found}, State}
  end.

get_room_message_history(RoomId, Messages) ->
  maps:get(RoomId, Messages, []).

uuid() ->
  make_ref().

call(Pid, Msg) ->
  Ref = erlang:monitor(process, Pid),
  Pid ! {Ref, self(), Msg},
  receive
    {reply, Ref, Reply} ->
      erlang:demonitor(Ref, [flush]),
      Reply;
    {'DOWN', Ref, process, Pid, Reason} ->
      {error, Reason}
  after 5000 ->
    erlang:demonitor(Ref, [flush]),
    noreply
  end.


loop(State0) ->
%%  io:format("~p enters loop ~n", [self()]),
  receive
    {Ref, From, Msg} -> {Reply, State1} = handle_call(Msg, State0),
      From ! {reply, Ref, Reply},
      ?MODULE:loop(State1);
    stop ->
        io:format("~p stops now ~n", [self()]),
        ok;
    Msg -> io:format("ERROR: ~p receive unknown msg ~p~n", [self(), Msg]),
      ?MODULE:loop(State0)
  end.
