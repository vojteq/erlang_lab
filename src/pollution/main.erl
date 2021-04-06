%%%-------------------------------------------------------------------
%%% @author vojteq
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. kwi 2021 14:46
%%%-------------------------------------------------------------------
-module(main).
-author("vojteq").

%% API
-export([start/0]).
-import(newPollution, [createMonitor/0, addStation/4, addValue/5, removeValue/4, getOneValue/4, getStationMean/3,
  getDailyMean/3, getDailyOverLimit/2, getMeasurementsOnStationInYear/3, getTheMostContaminatedStation/2,
  printMeas/1, printMonitor/1]).
-include_lib("records.hrl").

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
  printMeas(getOneValue(M7, "krakow", Time, Type)),
  io:format("MEAN: ~w~n", [getStationMean(M7, "krakow", "PM10")]),
  io:format("DAILY MEAN: ~w~n", [getDailyMean(M7, Time, Type)]),
  io:format("DAY OVER LIMIT (25.03): ~w~n", [getDailyOverLimit(M7, {2021, 3, 25})]),
  io:format("DAY OVER LIMIT (TODAY): ~w~n", [getDailyOverLimit(M7, element(1, Time))]),
  io:format("MEASUREMENTS IN 2021 on 'krakow' station: ~w~n", [getMeasurementsOnStationInYear(M7, "krakow", 2021)]).
