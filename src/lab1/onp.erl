%%%-------------------------------------------------------------------
%%% @author vojteq
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. mar 2021 13:22
%%%-------------------------------------------------------------------
-module(onp).
-author("vojteq").

%% API
-export([start/0, onp/1]).

start() ->
  % "2 + 3 * 5"
  io:fwrite(onp("2 3 5 * +")).

onp(Input) ->
  io:fwrite("ONP/1~n~s~n", [string:tokens(Input, " ")]),
  onp(string:tokens(Input, " "), []).

onp([], Result) ->
  io:fwrite("RESULT: ~s~n", [Result]);

onp(["+" | Tail], [First, Second | Stack]) ->
  {FirstNumber, _} = string:to_integer(First),
  {SecondNumber, _} = string:to_integer(Second),
  onp(Tail, [integer_to_list(SecondNumber + FirstNumber)] ++ Stack);

onp(["-" | Tail], [First, Second | Stack]) ->
  {FirstNumber, _} = string:to_integer(First),
  {SecondNumber, _} = string:to_integer(Second),
  onp(Tail, [integer_to_list(SecondNumber - FirstNumber)] ++ Stack);

onp(["*" | Tail], [First, Second | Stack]) ->
  {FirstNumber, _} = string:to_integer(First),
  {SecondNumber, _} = string:to_integer(Second),
  onp(Tail, [integer_to_list(SecondNumber * FirstNumber)] ++ Stack);

onp(["/" | Tail], [First, Second | Stack]) ->
  {FirstNumber, _} = string:to_integer(First),
  {SecondNumber, _} = string:to_integer(Second),
  onp(Tail, [integer_to_list(SecondNumber / FirstNumber)] ++ Stack);

onp([Head | Tail], Stack) ->
  onp(Tail, [Head] ++ Stack).