-module(example).
-export([start/0, ping/0]).

ping() ->
  receive
    {ping, Pong_PID} ->
      io:format("Ping!~n", []),
      Pong_PID!pong,
      ping()
  end.

start() ->
  Ping_PID = spawn(example, ping, []),
  Ping_PID!{ping, self()},
  receive
    {pong} ->
      io:format("Pong!~n", [])
  end.