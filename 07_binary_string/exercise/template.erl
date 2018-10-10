-module(template).

-export([parse/2, substitute/1, isKey/1]).

-include_lib("stdlib/include/ms_transform.hrl").

parse(Str, Data) when is_binary(Str) ->
  parse(split(Str), Data);

parse(BinaryList0, Data) when is_list(BinaryList0) andalso length(BinaryList0) >= map_size(Data) ->
  BinaryList1 = lists:map(
    fun(Part) ->
      case isKey(Part) of
        true ->
          io:format(" key: ~p~n", [Part]),
          substitute(maps:get(Part, Data, <<>>));
        false ->
          io:format(" not a key: ~p~n", [Part]),
          Part
      end
    end,
    BinaryList0
  ),
  binary_join(BinaryList1);


parse(BinaryList0, _Data) when is_list(BinaryList0) ->
  binary_join(BinaryList0).

substitute(<<>>) ->
  <<>>;

substitute(BinaryStr) when is_binary(BinaryStr) ->
  io:format(" BinaryStr: ~p~n", [BinaryStr]),
  BinaryStr;

substitute(NonBinaryStr) when is_list(NonBinaryStr) ->
  io:format(" BinaryStr: ~p~n", [NonBinaryStr]),
  unicode:characters_to_binary(NonBinaryStr);

substitute(Int) when is_integer(Int) ->
  io:format(" Int: ~p~n", [Int]),
  integer_to_binary(Int).


split(Str) when is_binary(Str) ->
  re:split(Str, <<"{{|}}">>,[{return, binary}]).

isKey(Subject) ->
  byte_size(Subject)-1 > 1 andalso  binary:at(Subject, 0) =/= 32 andalso binary:at(Subject, byte_size(Subject)-1) =/= 32 .

binary_join(BinaryList) ->
  F = fun(X, Y) -> <<X/binary, Y/binary>> end,
  lists:foldr(F, <<>>, BinaryList).