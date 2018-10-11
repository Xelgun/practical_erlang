-module(chat_user).
-behavior(gen_server).

-export([start_link/0, add_message/3, get_messages/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link(?MODULE, [], []).


add_message(UserPid, SenderName, Msg) ->
    gen_server:cast(UserPid, {add_message, {SenderName, Msg}}),
    ok.

get_messages(UserPid) ->
    gen_server:call(UserPid, get_messages).

init([]) ->
    {ok, []}.


handle_cast({add_message, Msg}, Messages) ->
    {noreply,[Msg | Messages]}.

handle_call(get_messages, _From, Messages) ->
    ReversedMessages = lists:reverse(Messages),
    {reply, ReversedMessages, Messages}.

