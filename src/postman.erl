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
-export([start/0]).
start()->
  io:format("\t- Starting Postman~n"),
  {ok, Postman} = python:start([{python_path, ?PYPATH}, {python, "python"}]),
  ResponseHandler = spawn(mpn_controller, on_receive_packet,[Postman]),
  python:call(Postman, postman, register_handler, [self(), ResponseHandler, ?SERIAL_PORT, ?BAUD_RATE]),
  register(mpnPostman, Postman),
  io:format("\t+ Postman Started!~n"),
  Postman.

