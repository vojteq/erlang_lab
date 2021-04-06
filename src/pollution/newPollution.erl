%%%-------------------------------------------------------------------
%%% @author vojteq
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. mar 2021 11:07
%%%-------------------------------------------------------------------
-module(newPollution).
-author("vojteq").

%% API
-export([createMonitor/0, addStation/4, addValue/5, removeValue/4, getOneValue/4, getStationMean/3, getDailyMean/3,
  getDailyOverLimit/2, getMeasurementsOnStationInYear/3, getTheMostContaminatedStation/2, printMeas/1, printMonitor/1]).
-include_lib("records.hrl").

createMonitor() ->
  [].

addStation(Monitor, StationName, Latitude, Longitude) when is_list(Monitor) andalso ?isStationName(StationName) andalso ?areCoords(Latitude, Longitude) ->
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

addValue(Monitor, StationName, {Date, Time}, Type, Value) when is_list(Monitor) andalso ?isStationName(StationName)
  andalso ?isDateOrTime(Date) andalso ?isDateOrTime(Time) andalso ?isType(Type) ->

  [case S#station.name of
     StationName ->
       M = #measurement{dateTime = #dateTime{date = Date, time = Time}, type = Type},
       Ms = S#station.measurements,
       S#station{measurements = Ms#{M => Value}};
     _ -> S
   end || S <- Monitor];
addValue(Monitor, Coords, {Date, Time}, Type, Value) when is_list(Monitor) andalso ?areCoords(Coords#coords.lat, Coords#coords.lon)
  andalso ?isDateOrTime(Date) andalso ?isDateOrTime(Time) andalso ?isType(Type) ->

  [case S#station.coords of
     Coords ->
       M = #measurement{dateTime = #dateTime{date = Date, time = Time}, type = Type},
       Ms = S#station.measurements,
       S#station{measurements = Ms#{M => Value}};
     _ -> S
   end || S <- Monitor].

removeValue(Monitor, StationName, {Date, Time}, Type) when is_list(Monitor) andalso ?isStationName(StationName)
  andalso ?isDateOrTime(Date) andalso ?isDateOrTime(Time) andalso ?isType(Type) ->

  [case S#station.name of
     StationName ->
       Ms = maps:remove(#measurement{dateTime = #dateTime{date = Date, time = Time}, type = Type}, S#station.measurements),
       S#station{measurements = Ms};
     _ -> S
   end || S <- Monitor];
removeValue(Monitor, Coords, {Date, Time}, Type) when is_list(Monitor) andalso ?areCoords(Coords#coords.lat, Coords#coords.lon)
  andalso ?isDateOrTime(Date) andalso ?isDateOrTime(Time) andalso ?isType(Type) ->

  [case S#station.coords of
     Coords ->
       Ms = maps:remove(#measurement{dateTime = #dateTime{date = Date, time = Time}, type = Type}, S#station.measurements),
       S#station{measurements = Ms};
     _ -> S
   end || S <- Monitor].

%% returns measurement from station with name==StationName, #measurement.date==Date and #measurement.type==Type
getOneValue(Monitor, StationName, {Date, Time}, Type) when is_list(Monitor) andalso ?isStationName(StationName)
  andalso ?isDateOrTime(Date) andalso ?isDateOrTime(Time) andalso ?isType(Type) ->

  FilterStations = fun(S) -> string:equal(S#station.name, StationName) end,
  FilterTypeAndDate = fun(#measurement{dateTime = #dateTime{date = D, time = T}, type = Ty}, _) ->
    string:equal(Ty, Type) andalso D == Date andalso Time == T end,
  Station = lists:nth(1, lists:filter(FilterStations, Monitor)),
  io:format("~w~n", [maps:to_list(maps:filter(FilterTypeAndDate, Station#station.measurements))]),
  maps:to_list(maps:filter(FilterTypeAndDate, Station#station.measurements)).

getStationMean(Monitor, StationName, Type) when is_list(Monitor) andalso ?isStationName(StationName) andalso ?isType(Type) ->
  FilterStations = fun(S) -> string:equal(S#station.name, StationName) end,
  FilterTypes = fun(#measurement{type = T, dateTime = _}, _) -> string:equal(T, Type) end,
  Station = lists:nth(1, lists:filter(FilterStations, Monitor)),
  Measurements = maps:to_list(maps:filter(FilterTypes, Station#station.measurements)),
  SumFun = fun({_, Val}, {Sum, Count}) -> {Sum + Val, Count + 1} end,
  {Sum, Count} = lists:foldl(SumFun, {0, 0}, Measurements),
  Sum / Count.

getDailyMean(Monitor, {Date, _}, Type) when is_list(Monitor) andalso ?isDateOrTime(Date) andalso ?isType(Type) ->
  SumFun = fun({Meas, Val}, {Sum, Count}) ->
    case Meas of
      #measurement{type = Type, dateTime = #dateTime{date = Date, time = _}} -> {Sum + Val, Count + 1};
      _ -> {Sum, Count}
    end end,
  ResultsFromStations = [lists:foldl(SumFun, {0, 0}, maps:to_list(S#station.measurements)) || S <- Monitor],
  SumAll = fun({Sum, Count}, {SumAll, CountAll}) -> {SumAll + Sum, CountAll + Count} end,
  {Summed, Counted} = lists:foldl(SumAll, {0, 0}, ResultsFromStations),
  Summed / Counted.

%% zwraca liczbę stacji, na których danego dnia co najmniej raz przekroczona jest norma wartości danego parametru;
%% Date = {Year,Month,Day}
getDailyOverLimit(Monitor, {Date, _}) when is_list(Monitor) andalso ?isDateOrTime(Date) andalso is_list(Monitor) ->
  CheckStation = fun(Measurements) ->
    Filter = fun(Measurement, V) -> Measurement#measurement.dateTime#dateTime.date == Date andalso V > 100 end,
    case maps:size(maps:filter(Filter, Measurements)) of
      0 -> 0;
      _ -> 1
    end
                 end,
  Fun = fun(Station, Counter) -> Counter + CheckStation(Station#station.measurements) end,
  lists:foldl(Fun, 0, Monitor).
%%getDailyOverLimit(Monitor, Date) when is_list(Monitor) andalso ?isDateOrTime(Date) andalso is_list(Monitor)->
%%  CheckStation = fun(Measurements) ->
%%    Filter = fun(Measurement, V) -> Measurement#measurement.dateTime#dateTime.date == Date andalso V > 100 end,
%%    case maps:size(maps:filter(Filter, Measurements)) of
%%      0 -> 0;
%%      _ -> 1
%%    end
%%  end,
%%  Fun = fun(Station, Counter) -> Counter + CheckStation(Station#station.measurements) end,
%%  lists:foldl(Fun, 0, Monitor).

%%LICZBA POMIARÓW WYKONANYCH NA DANEJ STACJI W DANYM ROKU
getMeasurementsOnStationInYear(Monitor, StationName, Year) when is_list(Monitor) andalso ?isStationName(StationName) andalso is_integer(Year)->
  [Station] = [S || S <- Monitor, string:equal(S#station.name, StationName)],
  Filter = fun(#measurement{dateTime = #dateTime{date = {Y, _, _}, time = _}, type = _}, _) -> Y == Year end,
  maps:size(maps:filter(Filter, Station#station.measurements)).

%% godzina kiedy srednia zanieczyszczen jest najwieksza
getTheMostContaminatedStation(Monitor, Hour) when is_list(Monitor) andalso is_integer(Hour) andalso 0 =< Hour andalso Hour =< 23 -> ok.
%%  GetMean = fun(Station) -> a end,
%%
%%  .

printMonitor(Monitor) when is_list(Monitor) ->
  io:format("~n*PRINTING MONITOR*"),
  printMonitor(Monitor, 1).

printMonitor([], _) ->
  io:format("~n*PRINTING MONITOR DONE*~n~n");
printMonitor([#station{name = Name, coords = #coords{lat = Lat, lon = Lon}, measurements = Measurements} | Tail], Counter) ->
  io:format("~n[~w] StationName: ~s, coords: ~w, ~w, measurements: ", [Counter, Name, Lat, Lon]),
  printMeas(maps:to_list(Measurements)),
  printMonitor(Tail, Counter + 1).

printMeas(List) when is_list(List) ->
  printMeas(List, 1).

printMeas([], _) ->
  ok;
printMeas([{#measurement{dateTime = #dateTime{date = Date, time = Time}, type = Type}, Value} | Tail], Counter) ->
  io:format("[~w]{date: ~w, time: ~w, type: ~s, value: ~w}, ", [Counter, Date, Time, Type, Value]),
  printMeas(Tail, Counter + 1).