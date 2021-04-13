%%%-------------------------------------------------------------------
%%% @author vojteq
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. kwi 2021 11:45
%%%-------------------------------------------------------------------
-module(distances).
-author("vojteq").

%% API
-export([start/0, findDistSeq/2, findDistAndSendBack/3, findDistProcesses/2, getLists/2, findDistOn4/2]).

-record(point, {x, y}).

start() ->
  Range = 10000,
  Lockers = 1000,
  People = 20000,
%%  PeopleLocations = [#point{x = rand:uniform(Range), y = rand:uniform(Range)} || _ <- lists:seq(1, Lockers)],
  PeopleLocationsLists = getLists(People div 4, 4),
  PeopleLocations = lists:umerge(PeopleLocationsLists),
  LockerLocations = [#point{x = rand:uniform(Range), y = rand:uniform(Range)} || _ <- lists:seq(1, Lockers)],
%%  {D, {P, L}} = findDistSeq(PeopleLocations, LockerLocations),
%%  io:format("D: ~w, P: ~w, L: ~w~n", [D, P, L]),
  {TimeSeq, ValSeq} = timer:tc(?MODULE, findDistSeq, [PeopleLocations, LockerLocations]),
  io:format("Sequential time: ~w, val: ~w~n", [TimeSeq, ValSeq]),
  {TimeProc, ValProc} = timer:tc(?MODULE, findDistProcesses, [PeopleLocations, LockerLocations]),
  io:format("A lot of processes time: ~w, val: ~w~n", [TimeProc, ValProc]),
  {Time4, Val4} = timer:tc(?MODULE, findDistOn4, [PeopleLocationsLists, LockerLocations]),
  io:format("A lot of processes time: ~w, val: ~w~n", [Time4, Val4]).


dist(Point1, Point2) ->
  math:sqrt(math:pow(Point1#point.x - Point2#point.x, 2) + math:pow(Point1#point.y - Point2#point.y, 2)).

%% sequential
findDistSeq(PeopleLocations, LockerLocations) ->
  ResultList = [{dist(X, Y), {X, Y}} || X <- PeopleLocations, Y <- LockerLocations],
  lists:min(ResultList).

% a lot of processes
findDistProcesses(PeopleLocations, LockerLocations) ->
  [spawn(?MODULE, findDistAndSendBack, [[PersonLocation], LockerLocations, self()]) || PersonLocation <- PeopleLocations],
  Result = [receive D -> D end || _ <- PeopleLocations],
  lists:min(Result).

findDistAndSendBack(PeopleLocations, LockerLocations, Pid) ->
  Pid ! findDistSeq(PeopleLocations, LockerLocations).

% 4 processes
findDistOn4(PeopleLocations, LockerLocations) ->
  [spawn(?MODULE, findDistSeq, [PLocations, LockerLocations, self()]) || PLocations <- PeopleLocations],
  Results = [receive D -> D end || _ <- PeopleLocations],
  Result = lists:umerge(Results),
  lists:min(Result).

getLists(Size, Number) ->
  [[rand:uniform(100) || _ <- lists:seq(1, Size)] || _ <- lists:seq(1, Number)].
