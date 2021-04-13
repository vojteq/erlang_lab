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
-export([createMonitor/0, addStation/4, addValue/5, removeValue/4, getOneValue/4, getStationMean/3, getDailyMean/3,
  getDailyOverLimit/2, getMeasurementsOnStationInYear/3, getTheMostContaminatedStation/2]).

%% strukturą danych jest lista zawierająca stacje pomiarowe, każda stacja przechowuje pomiary w postaci mapy,
%% w której kluczem jest {typ, {data, czas}} a wartpścią jest wartość pomiaru w procentach
-record(station, {name, coords, measurements = #{}}).
-record(coords, {lat, lon}).
-record(measurement, {type, dateTime}).
-record(dateTime, {date, time}).

-define(isStationName(Name), is_list(Name)).
-define(areCoords(Lat, Lon), (is_float(Lat) orelse is_integer(Lat)) andalso (is_float(Lon) orelse is_integer(Lon))).
-define(isType(Type), is_list(Type)).
-define(isDateOrTime(Input), is_tuple(Input) andalso tuple_size(Input) == 3 andalso is_integer(element(1, Input)) andalso is_integer(element(2, Input)) andalso is_integer(element(3, Input))).
-define(isHour(Hour), is_integer(Hour) andalso 0 =< Hour andalso Hour =< 23).

start() ->
  M = createMonitor(),
  printMonitor(M),
  M1 = addStation(M, "krakow", 30, 40),
  printMonitor(M1),
  M2 = addStation(M1, "warszawa", 12, 53),
  printMonitor(M2),
  M4 = addValue(M2, "krakow", calendar:local_time(), "PM10", 100),
  printMonitor(M4),
  timer:sleep(2000),
  M5 = addValue(M4, "krakow", calendar:local_time(), "PM5", 50),
  printMonitor(M5),
  timer:sleep(2000),
%%  M6 = addValue(M5, #coords{lat = 30, lon = 40}, calendar:local_time(), "PM10", 100),
  Time = calendar:local_time(),
  Type = "PM10",
  M6 = addValue(M5, #coords{lat = 30, lon = 40}, Time, Type, 200),
%%  M7 = addValue(M6, #coords{lat = 12, lon = 53}, Time, Type, 40),
  M7 = addValue(M6, "warszawa", Time, Type, 40),
  printMonitor(M7),
%%  printMeas(getAllOfType(M6, "krakow", "PM10")),
  printMeasurements(getOneValue(M7, "krakow", Time, Type)),
  io:format("MEAN: ~w~n", [getStationMean(M7, "krakow", "PM10")]),
  io:format("DAILY MEAN: ~w~n", [getDailyMean(M7, Time, Type)]),
%%  io:format("DAY OVER LIMIT (25.03): ~w~n", [getDailyOverLimit(M7, {2021, 3, 25})]),
  io:format("DAY OVER LIMIT (TODAY): ~w~n", [getDailyOverLimit(M7, element(1, Time))]),
  io:format("MEASUREMENTS IN 2021 at 'krakow' station: ~w~n", [getMeasurementsOnStationInYear(M7, "krakow", 2021)]),
  {StationName, Mean} = getTheMostContaminatedStation(M7, 17),
  io:format("MOST CONTAMINATED: ~s ~w~n", [StationName, Mean]).


createMonitor() ->
  [].

addStation(Monitor, StationName, Latitude, Longitude) when is_list(Monitor) andalso ?isStationName(StationName) andalso ?areCoords(Latitude, Longitude) ->
  NameTaken = fun(#station{name = Name, coords = _, measurements = _}, Boolean)
    -> (string:equal(StationName, Name) orelse Boolean) end,
  IsNameTaken = lists:foldl(NameTaken, false, Monitor),
  CoordsTaken = fun(#station{name = _, coords = #coords{lat = Lat, lon = Lon}, measurements = _}, Boolean) ->
    ((Lat =:= Latitude andalso Lon =:= Longitude) orelse Boolean) end,
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

%% liczba stacji, na których danego dnia co najmniej raz przekroczona jest norma wartości danego parametru
%% Date = {Year,Month,Day}
getDailyOverLimit(Monitor, Date) when is_list(Monitor) andalso ?isDateOrTime(Date) andalso is_list(Monitor) ->
  CheckStation = fun(Measurements) ->
    Filter = fun(Measurement, V) -> Measurement#measurement.dateTime#dateTime.date == Date andalso V > 100 end,
    case maps:size(maps:filter(Filter, Measurements)) of
      0 -> 0;
      _ -> 1
    end end,
  Fun = fun(Station, Counter) -> Counter + CheckStation(Station#station.measurements) end,
  lists:foldl(Fun, 0, Monitor).

%% liczba pomiarów wykonanych na danej stacji w danym roku
getMeasurementsOnStationInYear(Monitor, StationName, Year) when is_list(Monitor) andalso ?isStationName(StationName) andalso is_integer(Year) ->
  [Station] = [S || S <- Monitor, string:equal(S#station.name, StationName)],
  Filter = fun(#measurement{dateTime = #dateTime{date = {Y, _, _}, time = _}, type = _}, _) -> Y == Year end,
  maps:size(maps:filter(Filter, Station#station.measurements)).

%% stacja, na której średnie zanieczyszczenie o danej godzinie jest najwieksze (średnia ze wszystkich dni i wszystkich typów)
getTheMostContaminatedStation(Monitor, Hour) when is_list(Monitor) andalso is_integer(Hour) andalso ?isHour(Hour) ->
  SumMeasurements =
    fun({Measurement, Value}, {Sum, Count}) ->
      case Measurement#measurement.dateTime#dateTime.time of
        {Hour, _, _} -> {Sum + Value, Count + 1};
        _ -> {Sum, Count}
      end end,
  GetMeanFromStation = fun(Station) ->
    List = maps:to_list(Station#station.measurements),
    {Sum, Count} = lists:foldl(SumMeasurements, {0, 0}, List),
    case {Sum, Count} of
      {_, 0} -> {Station#station.name, 0};
      _ -> {Station#station.name, Sum / Count}
    end end,
  GetMostContaminated = fun({StationName, Mean}, {MostContStation, Highest}) ->
    case {Mean > Highest, Mean > 0} of
      {true, true} -> {StationName, Mean};
      _ -> {MostContStation, Highest}
    end end,
  List = [GetMeanFromStation(S) || S <- Monitor],
  lists:foldl(GetMostContaminated, {"no measurements", -1}, List).

printMonitor(Monitor) when is_list(Monitor) ->
  io:format("~n*PRINTING MONITOR*"),
  printMonitor(Monitor, 1).

printMonitor([], _) ->
  io:format("~n*PRINTING MONITOR DONE*~n~n");
printMonitor([#station{name = Name, coords = #coords{lat = Lat, lon = Lon}, measurements = Measurements} | Tail], Counter) ->
  io:format("~n[~w] StationName: ~s, coords: ~w, ~w, measurements: ", [Counter, Name, Lat, Lon]),
  printMeasurements(maps:to_list(Measurements)),
  printMonitor(Tail, Counter + 1).

printMeasurements(List) when is_list(List) ->
  printMeasurements(List, 1).

printMeasurements([], _) ->
  ok;
printMeasurements([{#measurement{dateTime = #dateTime{date = Date, time = Time}, type = Type}, Value} | Tail], Counter) ->
  io:format("[~w]{date: ~w, time: ~w, type: ~s, value: ~w}, ", [Counter, Date, Time, Type, Value]),
  printMeasurements(Tail, Counter + 1).