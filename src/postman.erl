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
-export([start/0, stop/0, restart/0, subscribe/1, unsubscribe/1, publish/1, tester/1]).
start()->
  io:format("\t- Starting Postman~n"),
  process_flag(trap_exit, true),
  {ok, Postman} = python:start([{python_path, ?PYPATH}, {python, "python"}]),
  python:call(Postman, postman, register_handler, [?SERIAL_PORT, ?BAUD_RATE]),
  %python:call(Postman, postman, on_serial, []),
  register(mpnPostman, Postman),
  io:format("\t+ Postman Started!~n"),
  Postman.

stop()->
  Postman = whereis(mpnPostman),
  case Postman of
    undefined  ->
      io:format("Process Not Exist~n");
    Other ->
      unregister(mpnPostman),
      python:stop(Other)
end.

restart()->
  io:format("Restart Initiated ..~n"),
  Postman = whereis(mpnPostman),
  case Postman of
    undefined  ->
      io:format("Process Not Exist~n");
    Other ->
      unregister(mpnPostman),
      python:stop(Other),
      timer:sleep(5000),
      try
          undefined = whereis(mpnPostman),
          start()
      catch
        exit: _ ->
           io:format("Process Running~n")
      end
end.

subscribe(Subscriber)->
  Data = python:call(whereis(mpnPostman), postman, subscribe, [Subscriber]),
  Data.

unsubscribe(Subscriber)->
  Data = python:call(whereis(mpnPostman), postman, unsubscribe, [Subscriber]),
  Data.


publish(Message)->
  Data = whereis(mpnPostman) ! Message,
  Data.

tester(ID)->
  receive
    Other->
      io:format("P~p <-- ~p~n", [ID, Other]),
      tester(ID)
  end.