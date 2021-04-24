%%%-------------------------------------------------------------------
%%% @author vojteq
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. kwi 2021 21:49
%%%-------------------------------------------------------------------
-module(pollution_test).
-author("vojteq").

-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
%% API
-export([]).

createMonitor_test() ->
  ?assertMatch([], pollution:createMonitor()).

addStation_test() ->
  P1 = pollution:createMonitor(),
  P2 = pollution:addStation(P1, "Station1", 30, 40),
  P3 = pollution:addStation(P2, "Station2", 20, 40),
  P4 = pollution:addStation(P3, "Station3", 12, 14),
  ?assertMatch({error, "name and coords taken"}, pollution:addStation(P4, "Station1", 30, 40)),
  ?assertMatch({error, "name taken"}, pollution:addStation(P4, "Station2", 8, 9)),
  ?assertMatch({error, "coords taken"}, pollution:addStation(P4, "Station10", 30, 40)).

addValue_test() ->
  P1 = pollution:createMonitor(),
  P2 = pollution:addStation(P1, "Station1", 30, 40),
  P3 = pollution:addValue(P2, "Station1", {{2014, 11, 20}, {11, 15, 0}}, "PM10", 56),
  ?assertMatch(P2, pollution:addValue(P2, "StationDoesNotExist", {{2014, 11, 20}, {11, 15, 0}}, "PM10", 56)),
%%  add existing measurement
  ?assertMatch(P3, pollution:addValue(P3, "Station1", {{2014, 11, 20}, {11, 15, 0}}, "PM10", 56)).

getOneValue_test() ->
  P1 = pollution:createMonitor(),
  P2 = pollution:addStation(P1, "Station1", 30, 40),
  P3 = pollution:addValue(P2, "Station1", {{2014, 11, 20}, {11, 15, 0}}, "PM10", 56),
  P4 = pollution:addValue(P3, "Station1", {{2014, 11, 20}, {11, 15, 0}}, "PM5", 56),
  P5 = pollution:addValue(P4, "Station1", {{2015, 11, 20}, {11, 15, 0}}, "PM10", 56),
  ?assertMatch({error, "station doesnt exist"}, pollution:getOneValue(P5, "StationDoesNotExist", {{2015, 11, 20}, {11, 15, 0}}, "PM10")),
  ?assertMatch({error, "no such measurement"}, pollution:getOneValue(P5, "Station1", {{2015, 11, 20}, {11, 15, 0}}, "PM11")).

getStationMean_test() ->
  P1 = pollution:createMonitor(),
  P2 = pollution:addStation(P1, "Station1", 30, 40),
  P3 = pollution:addValue(P2, "Station1", {{2014, 11, 20}, {11, 15, 0}}, "PM10", 10),
  P4 = pollution:addValue(P3, "Station1", {{2014, 11, 20}, {11, 15, 0}}, "PM5", 20),
  P5 = pollution:addValue(P4, "Station1", {{2015, 11, 20}, {11, 15, 0}}, "PM10", 60),
  P6 = pollution:addValue(P5, "Station1", {{2015, 11, 20}, {11, 15, 0}}, "PM2.5", 0),
  ?assertMatch(20.0, pollution:getStationMean(P6, "Station1", "PM5")),
  ?assertMatch(35.0, pollution:getStationMean(P6, "Station1", "PM10")),
  ?assertMatch(0.0, pollution:getStationMean(P6, "Station1", "PM2.5")),
  ?assertMatch({error, "station doesnt exist"}, pollution:getStationMean(P6, "StationDoesNotExist", "PM10")),
  ?assertMatch({error, "no measurements of this type"}, pollution:getStationMean(P6, "Station1", "PM1")).
