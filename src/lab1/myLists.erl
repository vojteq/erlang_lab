%%%-------------------------------------------------------------------
%%% @author vojteq
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. mar 2021 13:15
%%%-------------------------------------------------------------------
-module(myLists).
-author("vojteq").

%% API
-export([contains/2, duplicateElements/1, sumFloats/1]).

contains(_, []) ->
  false;
%contains(Element, [Head | _]) when Element == Head -> true; nizej prosciej
contains(Element, [Element | _]) ->
  true;
contains(Element, [_ | Tail]) ->
  contains(Element, Tail).


duplicateElements(List) when is_list(List)->
  duplicateElements(List, []);

duplicateElements(NotList) ->
  [NotList, NotList].

duplicateElements([], Acc) ->
  Acc;

duplicateElements([Head | Tail], Acc) ->
  duplicateElements(Tail, Acc ++ [Head, Head]).


sumFloats(List) when is_list(List) ->
  sumFloats(List, 0.0);

sumFloats(NotList) when is_float(NotList) ->
  NotList;

sumFloats(NotListOrFloat) ->
  0.0.

sumFloats([], Sum) ->
  Sum;

sumFloats([Head | Tail], Sum) when is_float(Head) ->
  sumFloats(Tail, Sum + Head);

sumFloats([_ | Tail], Sum) ->
  sumFloats(Tail, Sum).
