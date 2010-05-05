start_driver() ->
  Cmd = "rails runner ./lib/echo.rb",
  Port = open_port({spawn, Cmd}, [{packet, 4}, nouse_stdio,
            exit_status, binary]).