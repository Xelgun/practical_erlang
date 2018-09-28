-module(tic_tac_toe).

-export([new_game/0, win/1, move/3]).


new_game() ->
    {{f, f, f},
     {f, f, f},
     {f, f, f}}.


win({
  {o, _, _},
  {o, _, _},
  {o, _, _}
}) ->
  {win, o};


win({{_, o, _},
     {_, o, _},
     {_, o, _}}) ->
  {win, o};

win({{_, _, o},
     {_, _, o},
     {_, _, o}}) ->
  {win, o};

win({{o, o, o},
     {_, _, _},
     {_, _, _}}) ->
  {win, o};

win({{o, o, o},
     {_, _, _},
     {_, _, _}}) ->
  {win, o};

win({{_, _, _},
     {o, o, o},
     {_, _, _}}) ->
  {win, o};

win({{_, _, _},
     {_, _, _},
     {o, o, o}}) ->
  {win, o};

win({{o, _, _},
     {_, o, _},
     {_, _, o}}) ->
  {win, o};

win({{_, _, o},
     {_, o, _},
     {o, _, _}}) ->
  {win, o};

win({{x, f, f},
    {x, f, f},
    {x, f, f}}) ->
  {win, x};

win({{_, x, _},
     {_, x, _},
     {_, x, _}}) ->
  {win, x};

win({{_, _, x},
     {_, _, x},
     {_, _, x}}) ->
  {win, x};

win({{x, x, x},
     {_, _, _},
     {_, _, _}}) ->
  {win, x};

win({{x, x, x},
     {_, _, _},
     {_, _, _}}) ->
  {win, x};

win({{_, _, _},
     {x, x, x},
     {_, _, _}}) ->
  {win, x};

win({{_, _, _},
     {_, _, _},
     {x, x, x}}) ->
  {win, x};

win({{x, _, _},
     {_, x, _},
     {_, _, x}}) ->
  {win, x};

win({{_, _, x},
     {_, x, _},
     {x, _, _}}) ->
  {win, x};

win({{_, _, _},
     {_, _, _},
     {_, _, _}}) ->
  no_win.



move(Cell, Player, GameState) ->
  Map = stateToMap(GameState),
  case maps:get(Cell, Map, badkey) of
    f -> {ok, mapToState(Map#{Cell := Player})};
    _ -> {error, invalid_move}
  end.

stateToMap(GameState) ->
  {
    {_1, _2, _3},
    {_4, _5, _6},
    {_7, _8, _9}
  } = GameState,
  #{
    1 => _1,
    2 => _2,
    3 => _3,
    4 => _4,
    5 => _5,
    6 => _6,
    7 => _7,
    8 => _8,
    9 => _9
  }.

mapToState(Map) ->
  {
    {maps:get(1, Map), maps:get(2, Map), maps:get(3, Map)},
    {maps:get(4, Map), maps:get(5, Map), maps:get(6, Map)},
    {maps:get(7, Map), maps:get(8, Map), maps:get(9, Map)}
}.

