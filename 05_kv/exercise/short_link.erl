-module(short_link).

-export([init/0, create_short/2, get_long/2, rand_str/1]).

%%% module API

init() ->
    %% init randomizer
    <<A:32, B:32, C:32>> = crypto:strong_rand_bytes(12),
    rand:seed(exsp, {A,B,C}),
    State = #{long_to_short=> #{}, short_to_long=>#{}},
    State.


create_short(LongLink, State) ->
  LongToShort = maps:get(long_to_short, State),
  ShortToLong = maps:get(short_to_long, State),
  case maps:find(LongLink, LongToShort) of
    {ok, ShortLink} -> {ShortLink, State};
    _ ->  ShortLink = rand_str(string:length(LongLink)),
          {ShortLink, #{long_to_short => maps:put(LongLink, ShortLink, LongToShort), short_to_long => maps:put(ShortLink, LongLink, ShortToLong)}}
  end.

get_long(ShortLink, State) ->
  case maps:find(ShortLink, maps:get(short_to_long, State)) of
    {ok, LongLink} -> {ok, LongLink};
    _ -> {error, not_found}
  end.


%% generates random string of chars [a-zA-Z0-9]
rand_str(Length) ->
    lists:map(fun(Char) when Char > 83 -> Char + 13;
                 (Char) when Char > 57 -> Char + 7;
                 (Char) -> Char
              end,
              [rand:uniform(110 - 48) + 47 || _ <- lists:seq(1, Length)]).
