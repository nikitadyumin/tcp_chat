%%%-------------------------------------------------------------------
%%% @author Никита
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Окт. 2015 9:28
%%%-------------------------------------------------------------------
-module(server).
-author("Никита").

%% API
-export([listen/0]).

-define(TCP_OPTIONS, [list, {packet, 0}, {active, false}, {reuseaddr, true}, {ip, {0, 0, 0, 0}}]).

listen() ->
  {ok, LSock} = gen_tcp:listen(6667, ?TCP_OPTIONS),
  Server = spawn(fun() -> server_loop(maps:new()) end),
  tcp_connection_loop(Server, LSock).

tcp_connection_loop(Server, LSock) ->
  {ok, Socket} = gen_tcp:accept(LSock),
  Server ! {connected, Socket},
  spawn(fun() -> client_loop(Server, Socket, "") end),
  tcp_connection_loop(Server, LSock).

server_loop(Connections) ->
  receive
    {connected, Socket} -> server_loop(maps:put(Socket, "", Connections));
    {disconnected, Socket} -> server_loop(list_without(Socket, Connections));
    {name, Socket, Name} -> server_loop(maps:update(Socket, Name ++ ": ", Connections));
    {msg, Receiver, Data} ->
      sendout_message(list_without(Receiver, Connections), maps:get(Receiver, Connections) ++ Data),
      server_loop(Connections);
    {close} -> ok
  end.

list_without(Item, List) ->
  maps:remove(Item, List).

sendout_message(Connections, Msg) ->
  List = maps:keys(Connections),
  lists:map(fun(C) -> gen_tcp:send(C, Msg) end, List).

client_loop(Server, Socket, Acc) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, Packet} ->
      case Packet of
        [_Data | "\n"] ->
          parse_message(Server, Socket, (Acc ++ Packet)),
          client_loop(Server, Socket, "");
        Data -> client_loop(Server, Socket, Acc ++ Data)
      end;
    {error, _} ->
      Server ! {disconnected, Socket},
      ok
  end.

parse_message(Server, Client, Msg) ->
  case strip_eol(Msg) of
    "\\help" -> gen_tcp:send(Client, "available commands:\r\n help\r\n greetme\r\n name YOUR_NAME\r\n");
    "\\greetme" -> gen_tcp:send(Client, "hi\r\n");
    "\\name " ++ Name ->
      Server ! {name, Client, Name},
      gen_tcp:send(Client, "your name is: " ++ Name ++ "\r\n");
    "\\" ++ Cmd -> gen_tcp:send(Client, "invalid command: " ++ Cmd ++ "\r\n");
    _ -> Server ! {msg, Client, Msg}
  end.

strip_eol(Msg) ->
  string:strip(string:strip(Msg, both, $\n), both, $\r).
