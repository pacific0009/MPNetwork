%%%-------------------------------------------------------------------
%%% @author anand.ratna
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Aug 2019 11:09 AM
%%%-------------------------------------------------------------------
-module(mpn_controller).
-author("anand.ratna").
-include("config.hrl").
%% API
-export([start/0, on_receive_packet/1, prune_lost_bees/1]).
start()->
  io:format("-Starting MPN Controller\n"),
  mpn:start(),
  Postman = postman:start(),
  Pruner = spawn(mpn_controller, prune_lost_bees,[whereis(mpnPostman)]),
  io:format("+ MPN Controller Started\n"),
  ok.

handle_routing_packet(S, DATA)->
  io:format("$routing\n"),
  Dstn1 = list_to_integer(binary_to_list(string:slice(DATA,0,2)),16),
  Dist1 = list_to_integer(binary_to_list(string:slice(DATA,2,2)),16),
  Changed1 = mpn:update_routing_table(Dstn1, Dist1, S),
  Dstn2 = list_to_integer(binary_to_list(string:slice(DATA,4,2)),16),
  Dist2 = list_to_integer(binary_to_list(string:slice(DATA,6,2)),16),
  Changed2 = mpn:update_routing_table(Dstn2, Dist2,S),
  Changed1 or Changed2.

handle_bee_registration(Postman, S, DATA)->
  io:format("$registration"),
  {ok, MAC, BeeId} = mpn:register_bee(DATA),
  io:format("\t~p ~p ~p~n",[ok, MAC, BeeId]),
  NewMac = string:slice(MAC, 2),
  SR = string:pad(integer_to_list(BeeId, 16), 2, leading, "0"),
  Buff = lists:flatten(io_lib:format("~s~s", [list_to_binary(SR), NewMac])),
  Data = list_to_binary(Buff),
  Postman ! {2, ?MAX_BEES,S, ?MyId, Data},
  {ok, MAC, BeeId}.

update_bee_alive(Bee,MAC) ->
  %%Todo: Resolve Bee conflicts
  io:format("$sting"),
  mpn:alive_allocate_mpn_id(MAC, Bee),
  ok.

on_receive_packet(Postman)->
  receive
    {0,_,_,S,DATA} ->
      handle_routing_packet( S,DATA),
      on_receive_packet(Postman);
    {1,_,_,S,DATA} ->
      handle_bee_registration(Postman, S,DATA),
      on_receive_packet(Postman);
    {4,_,?MyId,S,DATA}->
      update_bee_alive(S,DATA),
      on_receive_packet(Postman);
    Other->
      io:format("$failed ~p~n", [Other]),
      on_receive_packet(Postman)
  end.

prune_lost_bees(Postman) ->
  timer:sleep(2000),
  Next_hop_bees = lists:usort(mpn:get_next_hop_bees()),
  lists:foreach(fun(Bee) ->
    bee_sting(Postman, Bee)
                end, Next_hop_bees),
  timer:sleep(2000),
  prune_lost_bees(Postman).

bee_sting(Postman, Bee)->
  if
    Bee == ?MyId -> io:format("\tBee~p \u2764~n",[Bee]);
    true ->
      Sting1 = is_bee_sting(Postman, Bee),
      if
        Sting1 -> io:format("Bee~p is alive~n",[Bee]);
        true ->
          Sting2 = is_bee_sting(Postman, Bee),
          if
            Sting2 -> io:format("Bee~p is alive~n",[Bee]);
            true ->
              mpn:set_bee_as_lost(Bee)
          end
      end
  end.

is_bee_sting(Postman, Bee)->
  {Bee,MAC} = lists:nth(1,mpn:get_registered_bee(Bee)),
  {_, Bee,_,NextHop} = lists:nth(1,mpn:get_distance_vector(Bee)),
  Postman ! {3, NextHop,Bee,?MyId, MAC},
  Send_time = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
  timer:sleep(1500),
  {_, Bee,_, _, LastActive, _} = lists:nth(1,mpn:get_mpn_for(Bee)),
  TimeDiff = calendar:datetime_to_gregorian_seconds(LastActive) - Send_time,
  TimeDiff >= 0.