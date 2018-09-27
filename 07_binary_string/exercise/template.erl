-module(template).

-export([parse/2, substitute/0, match/0]).

parse(Str, Data) when is_binary(Str) ->
%%    binary:split(Str, <<"{{">>)
    Str.

substitute(Str, ReplaceKey, ReplaceValue) when is_binary(Str)  ->
	NewString = re:replace(Str, ReplaceKey, ReplaceValue, [{return, list}]),
	io:format("~s~n",[NewString]).

match(Str) when is_binary(Str) ->
  M0 = #{<<"name">> => <<"Bob">>},
  case re:run(Str, "{{(\\w+)}}", [{capture, all, list}]) of
    {match,[RKey, ReValue]} -> ;
    _ -> ok
  end.