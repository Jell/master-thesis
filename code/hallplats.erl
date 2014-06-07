-module(hallplats).
-export([start/0, restart_driver/0, get/1]).

start() ->
  inets:start(),
  cache_server:start(),
  spawn(fun() -> start_driver() end).

start_driver() ->
  io:format("New Port opened from ~p~n", [self()]),
  Cmd = "ruby ./lib/echo.rb",
  Port = open_port({spawn, Cmd},
                   [{packet, 4}, nouse_stdio, exit_status, binary]),
  register(ruby_port, Port),
  DriverPid = spawn(fun() -> ruby_driver(Port) end),
  port_connect(Port, DriverPid),
  register(ruby_driver, DriverPid),
  ok.
  
restart_driver() ->
  exit(whereis(ruby_driver), kill),
  port_close(whereis(ruby_port)),
  start().

ruby_driver(Port) ->
  sqlquery:start(),
  port_connect(Port, self()),
  link(Port),
  receive
    {transmit, Payload, FromPid} ->
      FromPid!{ok, driver_call(Port, Payload)},
      ruby_driver(Port);
    {Port,{exit_status,_}} ->
      start();
    {'EXIT', Port, Reason} ->
      io:format("~p ~n", [Reason]),
      start()
  end.

driver_call(Port, Payload) ->
  port_command(Port, Payload),
  receive
      {Port, {data, Data}} ->
        {result, _Text} = binary_to_term(Data)
  end.

get(Params) ->
  MyPid = self(),
  Lat = proplists:get_value(lat, Params),
  Lng = proplists:get_value(lng, Params),
  StopList = sqlquery:fetch_nearby_stops(Lat,Lng),
  spawn(fun() -> fetch_forecasts(StopList, MyPid) end),
  receive
    {ok, {forecast, ForecastList}} ->
      %io_lib:format("~p~n", [ForecastList])
      ToParse = term_to_binary({parse, ForecastList}),
      ruby_driver!{transmit, ToParse, MyPid},
      receive
        {ok, {result, FinalResult}} ->
          FinalResult
      end
  end.

fetch_forecasts(StopList, ToPid) ->
  MyPid = self(),
  [spawn(fun() -> fetch_forecast(Stop, MyPid) end) || Stop <- StopList],
  ForecastList = wait_for_forecasts([]),
  ToPid!{ok,{forecast, ForecastList}}.

wait_for_forecasts(ForecastList) ->
  receive
    {ok, Forecast} ->
      if
        length(ForecastList) < 9 ->
          wait_for_forecasts([Forecast | ForecastList]);
        true ->
          _Result = [Forecast | ForecastList]
      end
  after
    5000 ->
      _Result = ForecastList
  end.

fetch_forecast(Stop, ToPid) ->
  {_,_,_,_,_,_,_,_,URL} = Stop,
  Body = cache_server:get(URL),
  ToPid!{ok, {Stop, Body}}.
  