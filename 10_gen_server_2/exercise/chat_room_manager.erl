-module(chat_room_manager).
-behavior(gen_server).

-export([start_link/0,
         create_room/1, get_rooms/0,
         add_user/3, remove_user/2, get_users/1,
         send_message/3,  get_history/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, handle_get_rooms/1]).

-define(add_room(Room), {add_room, Room}).

-define(get_rooms, get_rooms).

-define(add_user(RoomPid, UserName, UserPid), {RoomPid, add_user, [RoomPid, UserName, UserPid]}).

-define(remove_user(RoomPid, UserPid), {RoomPid, remove_user, [RoomPid, UserPid]}).

-define(get_users(RoomPid), {RoomPid, get_users, [RoomPid]}).

-define(send_message(RoomPid, SenderName, Msg), {RoomPid, send_message, [RoomPid, SenderName, Msg]}).

-define(get_history(RoomPid), {RoomPid, get_history, [RoomPid]}).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

create_room(RoomName) ->
  {ok, RoomPid} = chat_room:start_link(),
  Room = {RoomName, RoomPid},
  gen_server:cast(?MODULE, ?add_room(Room)),
  Room.

get_rooms() ->
  gen_server:call(?MODULE, ?get_rooms).

add_user(RoomPid, UserName, UserPid) ->
  gen_server:call(?MODULE, ?add_user(RoomPid, UserName, UserPid)).

remove_user(RoomPid, UserPid) ->
  gen_server:call(?MODULE, ?remove_user(RoomPid, UserPid)).

get_users(RoomPid) ->
  gen_server:call(?MODULE, ?get_users(RoomPid)).

send_message(RoomPid, SenderName, Msg) ->
  gen_server:call(?MODULE, ?send_message(RoomPid, SenderName, Msg)).

get_history(RoomPid) ->
  gen_server:call(?MODULE, ?get_history(RoomPid)).



init([]) ->
  {ok, #{}}.

handle_cast(?add_room(Room), Rooms0) ->
  {_RoomName, RoomPid} = Room,
  Rooms1 = maps:put(RoomPid, Room, Rooms0),
  {noreply, Rooms1}.

handle_call(?get_rooms, _From, Rooms) ->
  handle_get_rooms(Rooms);

handle_call({RoomPid, CallType, Args}, _From, Rooms) ->
  handle_room_call(RoomPid, CallType, Args, Rooms).

handle_get_rooms(Rooms) ->
  {reply, maps:values(Rooms), Rooms}.

handle_room_call(RoomPid, CallType, Args, Rooms) ->
  case maps:is_key(RoomPid, Rooms) of
    true -> {reply, handle_room_call(CallType, Args), Rooms};
    false -> {reply, {error, room_not_found}, Rooms}
  end.

handle_room_call(add_user, [RoomPid, UserName, UserPid] = _Args) ->
  handle_room_call_add_user(RoomPid, UserName, UserPid);

handle_room_call(remove_user, [RoomPid, UserPid] = _Args) ->
  handle_room_call_remove_user(RoomPid, UserPid);


handle_room_call(get_users, [RoomPid] = _Args) ->
  handle_room_call_get_users(RoomPid);


handle_room_call(send_message, [RoomPid, SenderName, Msg] = _Args) ->
  handle_room_call_send_message(RoomPid, SenderName, Msg);

handle_room_call(get_history, [RoomPid] = _Args) ->
  handle_room_call_get_history(RoomPid).

handle_room_call_add_user(RoomPid, UserName, UserPid) ->
  chat_room:add_user(RoomPid, UserName, UserPid),
  ok.

handle_room_call_remove_user(RoomPid, UserPid) ->
  chat_room:remove_user(RoomPid, UserPid).

handle_room_call_get_users(RoomPid) ->
  {ok, chat_room:get_users(RoomPid)}.

handle_room_call_send_message(RoomPid, SenderName, Msg) ->
  chat_room:add_message(RoomPid, SenderName, Msg).

handle_room_call_get_history(RoomPid) ->
  {ok, chat_room:get_history(RoomPid)}.


handle_info(_Request, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVersion, State, _Extra) ->
  {ok, State}.

