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
  % "2 + 3 * 5" = "2 3 5 * +" = 17
  % "1 + 2 + 3 + 4 + 5 + 6 * 7" = "1 2 + 3 + 4 + 5 + 6 7 * +" = 57
  io:fwrite("RESULT: ~w~n", [onp("1 2 + 3 + 4 + 5 + 6 7 * +")]).


onp(Input) ->
  onp(string:tokens(Input, " "), []).

onp([], [Result]) ->
  Result;

onp(["+" | Tail], [First, Second | Stack]) ->
  onp(Tail, [First + Second] ++ Stack);

onp(["-" | Tail], [First, Second | Stack]) ->
  onp(Tail, [First - Second] ++ Stack);

onp(["*" | Tail], [First, Second | Stack]) ->
  onp(Tail, [First * Second] ++ Stack);

onp(["/" | Tail], [First, Second | Stack]) ->
  onp(Tail, [Second / First] ++ Stack);

onp(["^" | Tail], [First, Second | Stack]) ->
  onp(Tail, [math:pow(Second, First)] ++ Stack);

onp(["sqrt" | Tail], [First | Stack]) ->
  onp(Tail, [math:sqrt(First)] ++ Stack);

onp(["sin" | Tail], [First | Stack]) ->
  onp(Tail, [math:sin(First)] ++ Stack);

onp(["cos" | Tail], [First | Stack]) ->
  onp(Tail, [math:cos(First)] ++ Stack);

onp(["tan" | Tail], [First | Stack]) ->
  onp(Tail, [math:tan(First)] ++ Stack);

onp([Head | Tail], Stack) ->
  onp(Tail, [getValue(Head)] ++ Stack).


getValue(StringValue) ->
  case string:to_float(StringValue) of
    {error, no_float} -> element(1, string:to_integer(StringValue));
    {_, _} -> element(1, string:to_float(StringValue))
  end.
