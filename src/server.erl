%%%-------------------------------------------------------------------
%%% @author Никита
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%% module implements a basic tcp chat
%%%   a server starts listening on all interfaces, port 6667
%%%   all the messages are resent to every connected client
%%%   it supports commands:
%%%   - \help           prints the list of commands
%%%   - \greetme [name] greets the user
%%%   - \quit           closes the client connection
%%%   - \shutdown       shuts down server
%%%   - \name [name]    changes the name of the connection
%%% @end
%%% Created : 17. Окт. 2015 9:28
%%%-------------------------------------------------------------------
-module(server).
-author("Никита").

%% API
-export([listen/0]).

-define(TCP_OPTIONS, [list, {packet, 0}, {active, false}, {reuseaddr, true}, {ip, {0, 0, 0, 0}}]).
-define(DEFAULT_NAME, "client").
-define(EMPTY_DATA, "").
-define(MESSAGE_DELIMITER, "\n").

listen() ->
  {ok, LSock} = gen_tcp:listen(6667, ?TCP_OPTIONS),
  Server = spawn(fun() -> server_loop(maps:new()) end),
  tcp_connection_loop(Server, LSock).

%%% @doc
%%% tcp loop
%%% on a new tcp connection
%%%   1. it registers the connection in a server loop
%%%   2. it creates a client loop
%%% @end
%%% @private
tcp_connection_loop(Server, LSock) ->
  {ok, Socket} = gen_tcp:accept(LSock),
  Server ! {connected, Socket},
  spawn(fun() -> client_loop(Server, Socket, ?EMPTY_DATA) end),
  tcp_connection_loop(Server, LSock).

%%% @doc
%%% main loop, that handles client connections
%%% `Connections` is a map that stores connection information
%%% (connection handler as key, "display name" as a value)
%%%
%%% @end
%%% @private
server_loop(Connections) ->
  receive
    {connected, Socket} -> server_loop(maps:put(Socket, ?DEFAULT_NAME, Connections));
    {disconnected, Socket} -> server_loop(maps:remove(Socket, Connections));
    {name, Socket, Name} -> server_loop(maps:update(Socket, Name, Connections));
    {msg, Receiver, Data} ->
      sendout_message(maps:remove(Receiver, Connections), maps:get(Receiver, Connections), Data),
      server_loop(Connections);
    {close} -> ok
  end.

%%% @doc
%%% sends out provided message under the given name to all the connections
%%% from the `Connections` map
%%%
%%% @end
%%% @private
sendout_message(Connections, Name, Data) ->
  List = maps:keys(Connections),
  Message = Name ++ ": " ++ Data,
  lists:map(fun(C) -> gen_tcp:send(C, Message) end, List).

%%% @doc
%%% client loop
%%% on input it accumulates data, splits it into chunks by "\n"
%%% and dispatches them via `parse_message`
%%%
%%% @end
%%% @private
client_loop(Server, Socket, Acc) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, Packet} ->
      case lists:reverse(Packet) of
        [?MESSAGE_DELIMITER | _Data] ->
          Commands = string:tokens(Acc ++ Packet, ?MESSAGE_DELIMITER),
          lists:map(fun(C)-> parse_message(Server, Socket, C ++ ?MESSAGE_DELIMITER) end, Commands),
          client_loop(Server, Socket, ?EMPTY_DATA);
        _ -> client_loop(Server, Socket, Acc ++ Packet)
      end;
    {error, _} ->
      Server ! {disconnected, Socket},
      ok
  end.

parse_message(Server, Client, Msg) ->
  case strip_eol(Msg) of
    "\\help" -> gen_tcp:send(Client, "available commands:\r\n help\r\n greetme\r\n name YOUR_NAME\r\n");
    "\\greetme" -> gen_tcp:send(Client, "hi\r\n");
    "\\quit " -> Server ! {disconnected, Client};
    "\\shutdown " -> Server ! {close};
    "\\name " ++ Name ->
      Server ! {name, Client, Name},
      gen_tcp:send(Client, "your name is: " ++ Name ++ "\r\n");
    "\\" ++ Cmd -> gen_tcp:send(Client, "invalid command: " ++ Cmd ++ "\r\n");
    _ -> Server ! {msg, Client, Msg}
  end.

strip_eol(Msg) ->
  string:strip(string:strip(Msg, both, $\n), both, $\r).
