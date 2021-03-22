%%%-------------------------------------------------------------------
%%% @author vojteq
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. mar 2021 11:30
%%%-------------------------------------------------------------------
-module(qsort).
-author("vojteq").

%% API
-export([qs/1, start/0, myMap/2, myFilter/2]).

start() ->
%%  io:fwrite("~w", [qs([1,6,3,5,87,12,2])]).
%%  io:fwrite("~w", [randomElems(20, 1, 5)]).
%%  List = randomElems(10000, 1, 10),
%%  compareSpeeds(List, fun qs/1, fun lists:sort/1),
  io:fwrite("~w", [sumDigits(1234)]).
%%  io:fwrite("~w", [sumDigits(1234)]).

qs([]) ->
  [];
qs([Pivot | Tail]) ->
  qs(lessThan(Tail, Pivot)) ++ [Pivot] ++ qs(grtEqThan(Tail, Pivot)).

lessThan(List, Arg) ->
  [X || X <- List, X < Arg].

grtEqThan(List, Arg) ->
  [X || X <- List, X >= Arg].

randomElems(N, Min, Max) ->
  [rand:uniform(Max - Min + 1) + Min - 1 || _ <- lists:seq(1, N)].

compareSpeeds(List, Fun1, Fun2) ->
  {Time1, _} = timer:tc(Fun1, [List]),
  {Time2, _} = timer:tc(Fun2, [List]),
  io:fwrite("time1 = ~w, time2 = ~w~n", [Time1, Time2]).


myMap(F, List) ->
  [F(X) || X <- List].
%qsort:myMap(fun (X) -> X * 4 end, lists:seq(1,10)).

myFilter(F, List) ->
  [X || X <- List, F(X)].
%qsort:myFilter(fun (X) -> X rem 2 == 0 end, lists:seq(1,10)).

sumDigits(N) ->
  lists:foldl(fun (X, Y) -> X + Y - $0 end, 0, integer_to_list(N)).