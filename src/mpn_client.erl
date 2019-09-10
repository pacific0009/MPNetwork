%%%-------------------------------------------------------------------
%%% @author anand.ratna
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Sep 2019 8:35 PM
%%%-------------------------------------------------------------------
-module(mpn_client).
-author("anand.ratna").

%% API
-export([start/0, stop/0, subscribe/1, unsubscribe/1, publish/1]).

start()->
  postman_service:start_link().

stop()->
  postman_service:stop().

subscribe(Subscriber)->
  postman_service:subscribe(Subscriber).

unsubscribe(Subscriber)->
  postman_service:unsubscribe(Subscriber).

publish(Message)->
  postman_service:publish(Message).

