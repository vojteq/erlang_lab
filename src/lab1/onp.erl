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
  io:fwrite("ONP/1~n"),
  onp(string:tokens(Input, " "), []).

onp([], Result) ->
  io:fwrite("RESULT~n"),
  Result;

onp(["+" | Tail], [First, Second | Stack]) ->
  io:fwrite("PLUS~n"),
  onp(Tail, [string:to_integer(First) + string:to_integer(Second)] ++ Stack);
onp(["-" | Tail], [First, Second | Stack]) ->
  io:fwrite("MINUS~n"),
  onp(Tail, [string:to_integer(First) - string:to_integer(Second)] ++ Stack);
onp(["*" | Tail], [First, Second | Stack]) ->
  io:fwrite("RAZY~n"),
  onp(Tail, [string:to_integer(First) * string:to_integer(Second)] ++ Stack);
onp(["/" | Tail], [First, Second | Stack]) ->
  io:fwrite("DZIELONE~n"),
  onp(Tail, [string:to_integer(First) / string:to_integer(Second)] ++ Stack);
onp([Head | Tail], Stack) ->
  io:fwrite("LICZBA~n"),
  onp(Tail, [Head] ++ Stack).

%%onp([Head | Rest], Stack) when is_number(Head)->
%%  onp(Rest, [Head] ++ Stack);
%%
%%onp([Head | Rest], [First, Second | Stack]) ->
%%  onp(Rest, [simpleOperation(Head, First, Second)] ++ Stack).
%%
%%
%%simpleOperation("+", First, Second) ->
%%  string:to_integer(First) + string:to_integer(Second);
%%
%%simpleOperation("-", First, Second) ->
%%  string:to_integer(First) - string:to_integer(Second);
%%
%%simpleOperation("*", First, Second) ->
%%  string:to_integer(First) * string:to_integer(Second);
%%
%%simpleOperation("/", First, Second) ->
%%  string:to_integer(First) / string:to_integer(Second).


