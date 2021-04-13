%%%-------------------------------------------------------------------
%%% @author vojteq
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. kwi 2021 18:52
%%%-------------------------------------------------------------------
-module(pollutionServer).
-author("vojteq").

-import(pollution, [createMonitor/0, addStation/4, addValue/5, removeValue/4, getOneValue/4, getStationMean/3,
getDailyMean/3, getDailyOverLimit/2, getMeasurementsOnStationInYear/3, getTheMostContaminatedStation/2]).

%% API
-export([start/0, init/0, stop/0, addStation/3, addValue/4, removeValue/3, getOneValue/3, getStationMean/2, getDailyMean/2,
  getDailyOverLimit/1, getMeasurementsOnStationInYear/2, getTheMostContaminatedStation/1, getMonitor/0]).

start() ->
  register(server, spawn(?MODULE, init, [])).

init() ->
  loop(createMonitor()).

stop() ->
  server ! stop.

loop(Monitor) ->
  receive
    {Pid, addStation, Name, Lat, Lon}  ->
      M = pollution:addStation(Monitor, Name, Lat, Lon),
      Pid ! "Station added",
      loop(M);
    {Pid, addValue, NameOrCoords, DateAndTime, Type, Value} ->
      M = pollution:addValue(Monitor, NameOrCoords, DateAndTime, Type, Value),
      Pid ! "Value added",
      loop(M);
    {Pid, removeValue, NameOrCoords, DateAndTime, Type} ->
      M = pollution:removeValue(Monitor, NameOrCoords, DateAndTime, Type),
      Pid ! "Value removed",
      loop(M);
    {Pid, getOneValue, Name, DateAndTime, Type} ->
      [Result] = pollution:getOneValue(Monitor, Name, DateAndTime, Type),
      Pid ! Result,
      loop(Monitor);
    {Pid, getStationMean, Name, Type} ->
      Result = pollution:getStationMean(Monitor, Name, Type),
      Pid ! Result,
      loop(Monitor);
    {Pid, getDailyMean, Date, Type} ->
      Result = pollution:getDailyMean(Monitor, Date, Type),
      Pid ! Result,
      loop(Monitor);
    {Pid, getDailyOverLimit, Date} ->
      Result = pollution:getDailyOverLimit(Monitor, Date),
      Pid ! Result,
      loop(Monitor);
    {Pid, getMeasurementsOnStationInYear, Name, Year} ->
      Result = pollution:getMeasurementsOnStationInYear(Monitor, Name, Year),
      Pid ! Result,
      loop(Monitor);
    {Pid, getTheMostContaminatedStation, Hour} ->
      Result = pollution:getTheMostContaminatedStation(Monitor, Hour),
      Pid ! Result,
      loop(Monitor);
    {Pid, getMonitor} ->
      Pid ! Monitor,
      loop(Monitor);
    {Pid, stop} ->
      Pid ! "Stopping server"
  after
    600000 -> "No actions -> stopping"
  end.




%% todo zwracanie jakichs bledow jak nie wyjda te funkcje
addStation(Name, Lat, Lon) ->
  server ! {self(), addStation, Name, Lat, Lon},
  receive
    Response -> Response
  end.

%% DateAndTime = {Date, Time}
addValue(NameOrCoords, DateAndTime, Type, Value) ->
  server ! {self(), addValue, NameOrCoords, DateAndTime, Type, Value},
  receive
    Response -> Response
  end.

%% DateAndTime = {Date, Time}
removeValue(NameOrCoords, DateAndTime, Type) ->
  server ! {self(), removeValue, NameOrCoords, DateAndTime, Type},
  receive
    Response -> Response
  end.

%% DateAndTime = {Date, Time}
getOneValue(Name, DateAndTime, Type) ->
  server ! {self(), getOneValue, Name, DateAndTime, Type},
  receive
    Response -> Response
  end.

getStationMean(Name, Type) ->
  server ! {self(), getStationMean, Name, Type},
  receive
    Response -> Response
  end.

getDailyMean(Date, Type) ->
  server ! {self(), getDailyMean, Date, Type},
  receive
    Response -> Response
  end.

getDailyOverLimit(Date) ->
  server ! {self(), getDailyOverLimit, Date},
  receive
    Response -> Response
  end.

getMeasurementsOnStationInYear(Name, Year) ->
  server ! {self(), getMeasurementsOnStationInYear, Name, Year},
  receive
    Response -> Response
  end.

getTheMostContaminatedStation(Hour) ->
  server ! {self(), getTheMostContaminatedStation,Hour},
  receive
    Response -> Response
  end.

getMonitor() ->
  server ! {self(), getMonitor},
  receive
    Response -> Response
  end.