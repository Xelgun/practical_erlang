-module(tic_tac_toe).

-export([new_game/0, win/1, move/3]).


new_game() ->
    {{f, f, f},
     {f, f, f},
     {f, f, f}}.


win({{o, f, f},
    {o, f, f},
    {o, f, f}}) ->
  {win, o}.


win({{_, o, _},
     {_, o, _},
     {_, o, _}}) ->
  {win, o}.
win({{_, _, o},
     {_, _, o},
     {_, _, o}}) ->
  {win, o}.
win({{o, o, o},
     {_, _, _},
     {_, _, _}}) ->
  {win, o}.
win({{o, o, o},
     {_, _, _},
     {_, _, _}}) ->
  {win, o}.
win({{_, _, _},
     {o, o, o},
     {_, _, _}}) ->
  {win, o}.

win({{_, _, _},
     {_, _, _},
     {o, o, o}}) ->
  {win, o}.

win({{o, _, _},
     {_, o, _},
     {_, _, o}}) ->
  {win, o}.

win({{_, _, o},
     {_, o, _},
     {o, _, _}}) ->
  {win, o}.
###
win({{x, f, f},
    {x, f, f},
    {x, f, f}}) ->
  {win, x}.


win({{_, x, _},
     {_, x, _},
     {_, x, _}}) ->
  {win, x}.
win({{_, _, x},
     {_, _, x},
     {_, _, x}}) ->
  {win, x}.
win({{x, x, x},
     {_, _, _},
     {_, _, _}}) ->
  {win, x}.
win({{x, x, x},
     {_, _, _},
     {_, _, _}}) ->
  {win, x}.
win({{_, _, _},
     {x, x, x},
     {_, _, _}}) ->
  {win, x}.

win({{_, _, _},
     {_, _, _},
     {x, x, x}}) ->
  {win, x}.

win({{x, _, _},
     {_, x, _},
     {_, _, x}}) ->
  {win, x}.

win({{_, _, x},
     {_, x, _},
     {x, _, _}}) ->
  {win, x}.


win({{_, _, _},
     {_, _, _},
     {_, _, _}}) ->
  no_win.



move(Cell, Player, GameState) ->
    {error, invalid_move}.
