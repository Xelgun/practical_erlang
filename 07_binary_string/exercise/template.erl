-module(template).

-export([parse/2]).

parse(Str, Data) when is_binary(Str) ->
  SplittedStr = split(Str),
  SplittedStr2 = lists:map(fun(Part) ->
    IsKey = string:strip(Part) =/=  Part,
    BinaryPart = unicode:characters_to_binary(Part),
    substitute(IsKey, maps:get(BinaryPart, Data, BinaryPart))
  end,
  SplittedStr),
  unicode:characters_to_binary(SplittedStr2).


substitute(false, Str) ->
  unicode:characters_to_binary(Str);

substitute(true, <<>>) ->
  <<>>;

substitute(true, BinaryStr) when is_binary(BinaryStr) ->
  BinaryStr;

substitute(true, NonBinaryStr) when is_list(NonBinaryStr) ->
  unicode:characters_to_binary(NonBinaryStr);

substitute(true, Str) when is_integer(Str) ->
  integer_to_binary(Str).



split(Str) when is_binary(Str) ->
  re:split(Str, <<"{{|}}">>,[{return, list}]).