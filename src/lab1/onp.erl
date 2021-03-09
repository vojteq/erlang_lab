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
%%  io:fwrite(onp("2 3 5 * +")).
  io:fwrite(onp("1.5 sin")).


onp(Input) ->
  onp(string:tokens(Input, " "), []).

onp([], Result) ->
  io:fwrite("RESULT: ~s~n", [Result]);

onp(["+" | Tail], [First, Second | Stack]) ->
  FirstNumber = get_value(First),
  SecondNumber = get_value(Second),
  onp(Tail, [number_to_list(SecondNumber + FirstNumber)] ++ Stack);

onp(["-" | Tail], [First, Second | Stack]) ->
  FirstNumber = get_value(First),
  SecondNumber = get_value(Second),
  onp(Tail, [number_to_list(SecondNumber - FirstNumber)] ++ Stack);

onp(["*" | Tail], [First, Second | Stack]) ->
  FirstNumber = get_value(First),
  SecondNumber = get_value(Second),
  onp(Tail, [number_to_list(SecondNumber * FirstNumber)] ++ Stack);

onp(["/" | Tail], [First, Second | Stack]) ->
  FirstNumber = get_value(First),
  SecondNumber = get_value(Second),
  onp(Tail, [number_to_list(SecondNumber / FirstNumber)] ++ Stack);

onp(["^" | Tail], [First, Second | Stack]) ->
  FirstNumber = get_value(First),
  SecondNumber = get_value(Second),
  onp(Tail, [number_to_list(math:pow(SecondNumber, FirstNumber))] ++ Stack);

onp(["sqrt" | Tail], [First | Stack]) ->
  Number = get_value(First),
  onp(Tail, [number_to_list(math:sqrt(Number))] ++ Stack);

onp(["sin" | Tail], [First | Stack]) ->
  Number = get_value(First),
  onp(Tail, [number_to_list(math:sin(Number))] ++ Stack);

onp(["cos" | Tail], [First | Stack]) ->
  Number = get_value(First),
  onp(Tail, [number_to_list(math:cos(Number))] ++ Stack);

onp(["tan" | Tail], [First | Stack]) ->
  Number = get_value(First),
  onp(Tail, [number_to_list(math:tan(Number))] ++ Stack);

onp([Head | Tail], Stack) ->
  onp(Tail, [Head] ++ Stack).


get_value(String) ->
  Float = (catch list_to_float(String)),
  Integer = (catch list_to_integer(String)),
  return_value(Integer, erlang:is_integer(Integer), Float, erlang:is_float(Float)).

return_value(Integer, true, _, false) ->
  Integer;
return_value(_, false, Float, true) ->
  Float.

number_to_list(Number) ->
  FloatList = (catch float_to_list(Number)),
  IntegerList = (catch integer_to_list(Number)),
  return_list(IntegerList, is_list(IntegerList), FloatList, is_list(FloatList)).

return_list(IntegerList, true, _, false) ->
  IntegerList;
return_list(_, false, FloatList, true) ->
  FloatList.