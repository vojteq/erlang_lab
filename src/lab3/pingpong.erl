%%%-------------------------------------------------------------------
%%% @author vojteq
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. kwi 2021 10:50
%%%-------------------------------------------------------------------
-module(pingpong).
-author("vojteq").

%% API
-export([start/0, stop/0, play/1, main/0, ping/0, pong/0]).

main() ->
  start(),
  play(10).

start() ->
  register(pingP, spawn(pingpong, ping, [])),
  register(pongP, spawn(pingpong, pong, [])).

stop() ->
  pingP ! stop,
  pongP ! stop.

play(N) ->
  io:format("starting [~w]~n", [N]),
  pingP ! N.

ping() ->
  timer:sleep(1000),
  receive
    stop ->
      io:format("stopping ping~n"),
      ok;
    0 ->
      io:format("ping done~n"),
      ping();
    N when is_integer(N) andalso N > 0 ->
      io:format("ping [~w]~n", [N]),
      pongP ! N - 1,
      ping()
  end.

pong() ->
  timer:sleep(1000),
  receive
    stop ->
      io:format("stopping pong~n"),
      ok;
    0 ->
      io:format("pong done~n"),
      ping();
    N when is_integer(N) andalso N > 0 ->
      io:format("pong [~w]~n", [N]),
      pingP ! N - 1,
      ping()
  end.