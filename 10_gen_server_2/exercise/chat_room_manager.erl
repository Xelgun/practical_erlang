-module(chat_room_manager).
-behavior(gen_server).

-export([start_link/0,
         create_room/1, get_rooms/0,
         add_user/3, remove_user/2, get_users/1,
         send_message/3,  get_history/1]).
-export([init/1, handle_call/3, handle_cast/2]).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

create_room(RoomName) ->
  {ok, RoomPid} = chat_room:start_link(),
  Room = {RoomName, RoomPid},
  gen_server:cast(?MODULE, {add_room, Room}),
  Room.

get_rooms() ->
  gen_server:call(?MODULE, get_rooms).

add_user(RoomPid, UserName, UserPid) ->
  gen_server:call(?MODULE, {add_user, {RoomPid, UserName, UserPid}}).

remove_user(RoomPid, UserPid) ->
  gen_server:call(?MODULE, {remove_user, {RoomPid, UserPid}}).

get_users(RoomPid) ->
  gen_server:call(?MODULE, {get_users, {RoomPid}}).

send_message(RoomPid, SenderName, Msg) ->
  gen_server:call(?MODULE, {send_message, {RoomPid, SenderName, Msg}}).

get_history(RoomPid) ->
  gen_server:call(?MODULE, {get_history, {RoomPid}}).



init([]) ->
  {ok, {}}.


handle_cast({add_room, Room}, Rooms0) ->
  {_RoomName, RoomPid} = Room,
  Rooms1 = maps:put(RoomPid, Room, Rooms0),
  {noreply, Rooms1}.


handle_call(get_rooms, _From, Rooms) ->
  {reply, maps:values(Rooms), Rooms};

handle_call({add_user, {RoomPid, UserName, UserPid}}, _From, Rooms) ->
  case maps:is_key(RoomPid, Rooms) of
    true  -> chat_room:add_user(RoomPid, UserName, UserPid),
             {reply, ok, Rooms};
    false -> {reply, {error, room_not_found}, Rooms}
  end;

handle_call({remove_user, {RoomPid, UserPid}}, _From, Rooms) ->
  case maps:is_key(RoomPid, Rooms) of
    true  -> {reply, chat_room:remove_user(RoomPid, UserPid), Rooms};
    false -> {reply, {error, room_not_found}, Rooms}
  end;

handle_call({get_users, {RoomPid}}, _From, Rooms) ->
  case maps:is_key(RoomPid, Rooms) of
    true ->  {reply, {ok, chat_room:get_users(RoomPid)}, Rooms};
    false -> {reply, {error, room_not_found}, Rooms}
  end;

handle_call({send_message,  {RoomPid, SenderName, Msg}}, _From, Rooms) ->
  case maps:is_key(RoomPid, Rooms) of
    true -> {reply, chat_room:add_message(RoomPid, SenderName, Msg), Rooms};
    false -> {reply, {error, room_not_found}, Rooms}
  end;

handle_call({get_history,  {RoomPid}}, _From, Rooms) ->
  case maps:is_key(RoomPid, Rooms) of
    true -> {reply, {ok, chat_room:get_history(RoomPid)}, Rooms};
    false -> {reply, {error, room_not_found}, Rooms}
  end.