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
-export([]).

%%komentarz o strukturach danych i ich przetwarzaniu


-record(station, {stationName, latitude, longitude}).
-record(measurement, {type, date, time}).
