%%%-------------------------------------------------------------------
%%% @author anand.ratna
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Sep 2019 7:24 PM
%%%-------------------------------------------------------------------
-module(postman).
-author("anand.ratna").
-include("config.hrl").
%% API
-export([start/0, stop/0, subscribe/1, unsubscribe/1, publish/1, tester/1]).
start()->
  io:format("\t- Starting Postman~n"),
  {ok, Postman} = python:start([{python_path, ?PYPATH}, {python, "python"}]),
  python:call(Postman, postman, register_handler, [self(), ?SERIAL_PORT, ?BAUD_RATE]),
  register(mpnPostman, Postman),
  io:format("\t+ Postman Started!~n"),
  Postman.

stop()->
  Postman = whereis(mpnPostman),
  unregister(Postman),
  python:stop(Postman),
  {ok, unregisterd}.

subscribe(Subscriber)->
  c:flush(),
  python:call(whereis(mpnPostman), postman, subscribe, [self(), Subscriber]),
  receive
    Data -> io:format("response:~p~n", [Data])
  end.

unsubscribe(Subscriber)->
  c:flush(),
  python:call(whereis(mpnPostman), postman, unsubscribe, [self(), Subscriber]),
  receive
    Data -> io:format("response:~p~n", [Data])
  end.


publish(Message)->
  c:flush(),
  whereis(mpnPostman) ! [self(), Message],
  receive
    Data -> io:format("response:~p~n", [Data])
  end.


tester(ID)->
  receive
    Other->
      io:format("$~p received:  ~p~n", [ID, Other]),
      tester(ID)
  end.