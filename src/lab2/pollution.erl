%%%-------------------------------------------------------------------
%%% @author vojteq
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. mar 2021 11:07
%%%-------------------------------------------------------------------
-module(pollution).
-author("vojteq").

%% API
-export([start/0]).

-record(station, {name, coords, measurements = #{}}).
-record(coords, {lat, lon}).
-record(measurement, {type, date}).

-define(isStationName(Name), is_list(Name)).
-define(areCoords(Lat, Lon), (is_float(Lat) orelse is_integer(Lat)) andalso (is_float(Lon) orelse is_integer(Lon))).

start() ->
  M = createMonitor(),
  printMonitor(M),
  M1 = addStation(M, "krakow", 30, 40),
  printMonitor(M1),
  M2 = addStation(M1, "warszawa", 12, 53),
  printMonitor(M2),
  M4 = addValue(M2, "krakow", calendar:local_time(), "PM10", 100),
  printMonitor(M4),
  timer:sleep(5000),
  M5 = addValue(M4, "krakow", calendar:local_time(), "PM10", 100),
  printMonitor(M5),
  timer:sleep(5000),
  M6 = addValue(M5, #coords{lat = 30, lon = 40}, calendar:local_time(), "PM10", 100),
  printMonitor(M6).


createMonitor() ->
  [].

addStation(Monitor, StationName, Latitude, Longitude) when ?isStationName(StationName) andalso ?areCoords(Latitude, Longitude) ->
  NameTaken = fun(#station{name = Name, coords = _, measurements = _}, Boolean)
    -> (string:equal(StationName, Name) orelse Boolean) end,
  IsNameTaken = lists:foldl(NameTaken, false, Monitor),

  CoordsTaken = fun(#station{name = _, coords = #coords{lat = Lat, lon = Lon}, measurements = _}, Boolean)
    -> ((Lat =:= Latitude andalso Lon =:= Longitude) orelse Boolean) end,
  AreCoordsTaken = lists:foldl(CoordsTaken, false, Monitor),

  case {IsNameTaken, AreCoordsTaken} of
    {true, true} ->
      io:format("ERROR: Name: '~s' and coords: (~w, ~w) are taken~n~n", [StationName, Latitude, Longitude]),
      Monitor;
    {true, false} ->
      io:format("ERROR: Name: '~s' is taken~n~n", [StationName]),
      Monitor;
    {false, true} ->
      io:format("ERROR: Coords: (~w, ~w) are taken~n~n", [Latitude, Longitude]),
      Monitor;
    {false, false} ->
      [#station{name = StationName, coords = #coords{lat = Latitude, lon = Longitude}, measurements = #{}} | Monitor]
  end.

addValue(Monitor, StationName, Date, Type, Value) when is_list(StationName) ->
  [case S#station.name of
     StationName ->
       M = #measurement{date = Date, type = Type},
       Ms =  S#station.measurements,
       S#station{measurements = Ms#{M => Value}};
     _ -> S
   end || S <- Monitor];
addValue(Monitor, Coords, Date, Type, Value) when ?areCoords(Coords#coords.lat, Coords#coords.lon) ->
  [case S#station.coords of
     Coords ->
       M = #measurement{date = Date, type = Type},
       Ms =  S#station.measurements,
       S#station{measurements = Ms#{M => Value}};
     _ -> S
   end|| S <- Monitor].


printMonitor(Monitor) when is_list(Monitor) ->
  io:format("~n*PRINTING MONITOR*"),
  printMonitor(Monitor, 0).

printMonitor([], _) ->
  io:format("~n*PRINTING MONITOR DONE*~n~n");
printMonitor([#station{name = Name, coords = #coords{lat = Lat, lon = Lon}, measurements = Measurements} | Tail], Counter) ->
  io:format("~n[~w] StationName: ~s, coords: ~w, ~w, measurements: ", [Counter, Name, Lat, Lon]),
  printMeas(maps:to_list(Measurements)),
  printMonitor(Tail, Counter + 1).

printMeas([]) ->
  ok;
printMeas([{#measurement{date = Date, type = Type}, Value} | Tail]) ->
  io:format("{date: ~w, type: ~s, value: ~w%}, ", [Date, Type, Value]),
  printMeas(Tail).