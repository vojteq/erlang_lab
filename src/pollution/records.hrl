%%%-------------------------------------------------------------------
%%% @author vojteq
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. kwi 2021 14:57
%%%-------------------------------------------------------------------
-author("vojteq").


-record(station, {name, coords, measurements = #{}}).
-record(coords, {lat, lon}).
-record(measurement, {type, dateTime}).
-record(dateTime, {date, time}).


-define(isStationName(Name), is_list(Name)).
-define(areCoords(Lat, Lon), (is_float(Lat) orelse is_integer(Lat)) andalso (is_float(Lon) orelse is_integer(Lon))).
%%-define(isType(Type), is_list(Type) andalso (string:equal(Type, "PM10") orelse string:equal(Type, "PM5") orelse string:equal(Type, "PM2.5"))).   //TODO czy da sie to zrobic?
%%-define(isType(Type), is_list(Type) andalso (Type == "PM10") orelse (Type == "PM5") orelse (Type == "PM2.5"))).
-define(isType(Type), is_list(Type)).
-define(isDateOrTime(Input), is_tuple(Input) andalso tuple_size(Input) == 3 andalso is_integer(element(1, Input)) andalso is_integer(element(2, Input)) andalso is_integer(element(3, Input))).