-module(sqlquery).
-export([start/0, fetch_nearby_stops/2, distance_geodesic/4]).

start() ->
  % sqlite:start_link(development),
  sqlite:start_link(development,
                    [{db, "../Rails/db/development.sqlite3"}]),
  sqlite:list_tables().
  
fetch_nearby_stops(Lat, Lng) ->
  Area = 0.002,
  Ratio = distance_geodesic(Lat, Lng, Lat, Lng + 0.1) /
                      distance_geodesic(Lat, Lng, Lat + 0.1, Lng),

  Query = io_lib:format( "SELECT * FROM bus_stops WHERE
                               lat > ~p AND lat < ~p AND
                               lng > ~p AND lng < ~p LIMIT 50",
                          [Lat - Ratio*Area,
                           Lat + Ratio*Area, 
                           Lng - Area,
                           Lng + Area]),

  case sqlite:sql_exec(Query) of
    ok ->
      Result = find_10_closer([], Area * 2, Ratio, Lat, Lng);
    List ->
      if
        length(List) > 10 ->
          Result = List;
        true ->
          Result = find_10_closer(List, Area * 2, Ratio, Lat, Lng)
      end
  end,
  lists:sublist(sort_result(Result, Lat, Lng), 10).

sort_result(Result, Lat, Lng) ->
  lists:sort(
        fun(A, B) ->
          {_,_,Lat1,Lng1,_,_,_,_,_} = A,
          {_,_,Lat2,Lng2,_,_,_,_,_} = B,
          {A1, []} = string:to_float(Lat1),
          {B1, []} = string:to_float(Lng1),
          {A2, []} = string:to_float(Lat2),
          {B2, []} = string:to_float(Lng2),
          distance_geodesic(Lat, Lng, A1, B1) =<
                                     distance_geodesic(Lat, Lng, A2, B2)
        end, Result).

distance_geodesic(Lat1, Long1, Lat2, Long2) ->
    A1 = Lat1 * (math:pi() / 180),
    B1 = Long1 * (math:pi() / 180),
    A2 = Lat2 * (math:pi() / 180),
    B2 = Long2 * (math:pi() / 180),
    R = 6356.75,
    R * math:acos(math:cos(A1)*math:cos(B1)*math:cos(A2)*math:cos(B2) + 
                  math:cos(A1)*math:sin(B1)*math:cos(A2)*math:sin(B2) +
                  math:sin(A1)*math:sin(A2)).

find_10_closer(OldList, Area, Ratio, Lat, Lng) ->
  Query = io_lib:format("SELECT * FROM bus_stops WHERE lat > ~p AND
                            lat < ~p AND lng > ~p AND lng < ~p AND NOT
                                 (lat > ~p AND lat < ~p AND
                                  lng > ~p AND lng < ~p) LIMIT 50",
                          [Lat - 2 * Ratio * Area,
                           Lat + 2 * Ratio * Area,
                           Lng - 2 * Area, Lng + 2 * Area,
                           Lat - Ratio * Area,
                           Lat + Ratio * Area,
                           Lng - Area,
                           Lng + Area]),

  Result = sqlite:sql_exec(Query),
  case Result of
    ok ->
      find_10_closer(OldList, Area * 2, Ratio, Lat, Lng);
    List ->
      NewList = erlang:append(List, OldList),
      if
        length(NewList) > 10 ->
          NewList;
        true ->
          find_10_closer(NewList, Area * 2, Ratio, Lat, Lng)
      end  
  end.