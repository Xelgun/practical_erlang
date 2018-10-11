-module(chat_room).
-behavior(gen_server).

-export([start_link/0, add_user/3, remove_user/2, get_users/1, add_message/3, get_history/1]).
-export([init/1, handle_call/3, handle_cast/2]).


start_link() ->
  gen_server:start_link(?MODULE, [], []).

add_user(RoomPid, UserName, UserPid) ->
  gen_server:cast(RoomPid, {add_user, {UserName, UserPid}}),
  ok.

remove_user(RoomPid, UserPid) ->
  gen_server:call(RoomPid, {remove_user, UserPid}).


get_users(RoomPid) ->
  gen_server:call(RoomPid, get_users).


add_message(RoomPid, UserName, Msg) ->
  gen_server:cast(RoomPid, {add_message, {UserName, Msg}}),
  ok.


get_history(RoomPid) ->
  gen_server:call(RoomPid, get_history).

init([]) ->
  {ok, {{},[]}}.

handle_cast({add_user, User}, {Users0, Messages} = _State) ->
  {_UserName, UserPid} = User,
  Users1 = maps:put(UserPid, User, Users0),
  {noreply, {Users1, Messages}};

handle_cast({add_message, {UserName, Msg}}, {Users, Messages0} = _State) ->
  lists:foreach(
    fun(UserPid) ->
      chat_user:add_message(UserPid, UserName, Msg)
    end,
    maps:keys(Users)
  ),
  Messages1 = [{UserName, Msg} | Messages0],
  {noreply, {Users, Messages1}}.

handle_call({remove_user, UserPid}, _From, {Users0, Messages} = State) ->
  case maps:take(UserPid, Users0) of
    {_User, Users1} -> {reply, ok, {Users1, Messages}};
    error -> {reply, {error, user_not_found}, {State}}
  end;

handle_call(get_users, _From, {Users, _Messages} = State) ->
  {reply, maps:values(Users), State};

handle_call(get_history, _From, {_Users, Messages} = State) ->
  {reply, lists:reverse(Messages), State}.

