%%%-------------------------------------------------------------------
%%% @author vojteq
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. mar 2021 21:09
%%%-------------------------------------------------------------------
-module(main).
-author("vojteq").

%% API
-export([start/0]).

start() ->
  %io:fwrite("hello world\n"),
  %loop(10).
  %io:fwrite("~w\n", [power(20, 2)]),
  List = [1,2,3,4,5],
  io:fwrite("~w\n", [myLists:contains(3, List)]).

%4.1 {Name, Time, [{"PM10", value}, {"PM2.5", value}...]}
%4.2
% P1 = {"station1", time(), [{"PM10", 150}]}.
% P2 = {"station2", time(), [{"PM10", 90}, {"PM2.5", 600}]}.
% P3 = {"station3", time(), ["temperature", 20]}.
%4.3
% ListaPomiarow = [P1, P2, P3].
%4.4
% P4 = {"station4", time(), [{"PM10", 10}]}.
%4.5
% NowaListaPomiarow = ListaPomiarow ++ P4.
%4.6
% {NazwaP1, _, _} = P1.

power(Number, Power) ->
  power(Number, Power, 1).
power(_, 0, _) ->
  1;
power(Number, 1, Result) ->
  Result * Number;
power(Number, Power, Result) ->
  power(Number, Power - 1, Result * Number).