-module(cache_server).
-export([start/0, get/1, cleaner_daemon/0]).
-record(cached_url, {url, timestamp, data}).

start() ->
  mnesia:start(),
  mnesia:create_table(cached_url, 
                     [{attributes, record_info(fields, cached_url)}]),
  spawn(fun() -> cleaner_daemon() end).

cleaner_daemon() ->
  F = fun() ->
      {Mega, Sec, _Micro} = erlang:now(),
      T = Mega * 1000000 + Sec - 60,
      TimeStamp = {T div 1000000, T rem 1000000, 0},
      MatchHead = #cached_url{url = '$1', timestamp = '$2', data = '_'},
      io:format("~p~n", [TimeStamp]),
      Guard = {'<', '$2', {TimeStamp}},
      Result = '$1',
      ToDelete = mnesia:select(cached_url,
                               [{MatchHead, [Guard], [Result]}]),
      [ mnesia:delete({cached_url, Url}) || Url<-ToDelete]
    end,
  receive
  after
    600000 ->
      mnesia:transaction(F),
      cleaner_daemon()
  end.  

get(Url) ->
  case request_mnesia(Url) of
    {atomic,[[TimeStamp,DataTmp]]} ->
      case is_timestamp_valid(TimeStamp) of
        true ->
          Data = DataTmp;
        false ->
          Data = get_over_http(Url, 3),
          spawn(fun() -> write_mnesia(Url, Data) end)
      end;
    _ ->
      Data = get_over_http(Url, 3),
      spawn(fun() -> write_mnesia(Url, Data) end)
  end,
  Data.

get_over_http(Url, Retries) ->
  {ok, RequestId} = http:request(get, {Url, []}, [], [{sync, false}]),
  receive
    {http, {RequestId, Result}} ->
      case Result of
        {{"HTTP/1.1",200,"OK"}, Header, Body} ->
          UglyTest = [ 'TEST'|| {"content-length", "2442"} <- Header],
          case UglyTest of
            [] ->
              Body;
            _ ->
              if
                Retries > 1 ->
                  get_over_http(Url, Retries - 1)
              end
          end;
        _ ->
          if
            Retries > 1 ->
              get_over_http(Url, Retries - 1)
          end
      end
  after
    3000 ->
      if
        Retries > 1 ->
          get_over_http(Url, Retries - 1)
      end
  end.

is_timestamp_valid({MegaSec, Sec, _MicroSec}) ->
  TimeStamp = MegaSec*1000000+Sec,
  {NowMega, NowSec, _NowMicro} = erlang:now(),
  NowStamp = NowMega*1000000+NowSec,
  NowStamp - TimeStamp < 45.

request_mnesia(Url) ->
  F = fun() ->
      Cache = #cached_url{url = Url, timestamp = '$1', data = '$2'},
      mnesia:select(cached_url, [{Cache, [], [['$1', '$2']]}])
    end,
  mnesia:transaction(F).
  
write_mnesia(Url, Data) ->
  Cache  = #cached_url{  url= Url,
              timestamp = erlang:now(),
                         data = Data},
  F = fun() ->
      mnesia:write(Cache)
    end,
  mnesia:transaction(F).