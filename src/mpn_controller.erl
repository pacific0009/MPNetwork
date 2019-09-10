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
-export([start/0, on_receive_packet/0, prune_lost_bees/0, response_handler/1]).
start()->
  io:format("- Starting MPN Controller\n"),
%%  Handler = spawn(mpn_controller, on_receive_packet,[]),
%%  postman_service:subscribe(Handler),
%%  Pruner = spawn(mpn_controller, prune_lost_bees,[]),
  io:format("+ MPN Controller Started\n"),
  ok.

handle_routing_packet(S, DATA)->
  io:format("-*-routing\n"),
  Dstn1 = list_to_integer(binary_to_list(string:slice(DATA,0,2)),16),
  Dist1 = list_to_integer(binary_to_list(string:slice(DATA,2,2)),16),
  Changed1 = mpn_service:update_routing_table(Dstn1, Dist1, S),
  Dstn2 = list_to_integer(binary_to_list(string:slice(DATA,4,2)),16),
  Dist2 = list_to_integer(binary_to_list(string:slice(DATA,6,2)),16),
  Changed2 = mpn_service:update_routing_table(Dstn2, Dist2,S),
  Changed1 or Changed2.

handle_bee_registration( S, DATA)->
  io:format("-*-registration"),
  {ok, MAC, BeeId} = mpn_service:register_bee(DATA),
  io:format("\t~p ~p ~p~n",[ok, MAC, BeeId]),
  NewMac = string:slice(MAC, 2),
  SR = string:pad(integer_to_list(BeeId, 16), 2, leading, "0"),
  Buff = lists:flatten(io_lib:format("~s~s", [list_to_binary(SR), NewMac])),
  Data = list_to_binary(Buff),
  postman_service:publish({2, ?MAX_BEES,S, ?MyId, Data}),
  {ok, MAC, BeeId}.

update_bee_alive(Bee,MAC) ->
  %%Todo: Resolve Bee conflicts
  io:format("-*-sting"),
  mpn_service:alive_allocate_mpn_id(MAC, Bee),
  ok.

register_services(Bee, DATA)->
  {Bee,MAC} = lists:nth(1,mpn_service:get_registered_bee(Bee)),
  S1 = list_to_integer(binary_to_list(string:slice(DATA,0,2)),16),
  S2 = list_to_integer(binary_to_list(string:slice(DATA,2,2)),16),
  S3 = list_to_integer(binary_to_list(string:slice(DATA,4,2)),16),
  S4 = list_to_integer(binary_to_list(string:slice(DATA,6,2)),16),
  mpn_service:register_bee_services(MAC, S1, S2, S3, S4),
  ok.

store_response(Bee, DATA)->
  {Bee,MAC} = lists:nth(1,mpn_service:get_registered_bee(Bee)),
  RC =  list_to_integer(binary_to_list(string:slice(DATA,0,2)),16),
  Service = list_to_integer(binary_to_list(string:slice(DATA,2,2)),16),
  Response = list_to_integer(binary_to_list(string:slice(DATA,4,8)),16),
  mpn_service:store_response(MAC, Service, Response).

response_handler(Data)->
  case Data of
    {0,_,_,S,DATA} ->
      handle_routing_packet( S,DATA);
    {1,_,_,S,DATA} ->
      handle_bee_registration( S,DATA);
    {4,_,?MyId,S,DATA}->
      update_bee_alive(S,DATA);
    Other->
      io:format("$failed ~p~n", [Other])
  end.




on_receive_packet()->
  receive
    {0,_,_,S,DATA} ->
      handle_routing_packet( S,DATA),
      on_receive_packet();
    {1,_,_,S,DATA} ->
      handle_bee_registration( S,DATA),
      on_receive_packet();
    {2,_,_,S,DATA} ->
      %% ignore
      on_receive_packet();
    {3,_,_,S,DATA} ->
      %% ignore
      on_receive_packet();
    {4,_,?MyId,S,DATA}->
      update_bee_alive(S,DATA),
      on_receive_packet();
    {5,_,_,S,DATA} ->
      %% ignore
      on_receive_packet();
    {6,_,?MyId,S,DATA}->
      register_services(S, DATA),
      on_receive_packet();
    {_,_,?MyId,S,DATA}->
      store_response(S, DATA),
      on_receive_packet();
    Other->
      io:format("$failed ~p~n", [Other]),
      on_receive_packet()
  end.

prune_lost_bees() ->
  Next_hop_bees = lists:usort(mpn_service:get_next_hop_bees()),
  lists:foreach(fun(Bee) ->
    bee_sting(Bee), timer:sleep(500)
                end, Next_hop_bees).


bee_sting(Bee)->
  BHeart = [{field1, unicode:characters_to_binary("♥")}],
  [{field1, Heart}|_] = BHeart,
  if
    Bee == ?MyId -> io:format("\tBee(~p) ~ts~n",[Bee, Heart]);
    true ->
      Sting1 = is_bee_sting(Bee),
      if
        Sting1 -> io:format("Bee(~p) ~ts~n",[Bee, Heart]);
        true ->
          Sting2 = is_bee_sting( Bee),
          if
            Sting2 -> io:format("Bee(~p) ~ts~n",[Bee, Heart]);
            true ->
              mpn_service:set_bee_as_lost(Bee)
          end
      end
  end.

is_bee_sting( Bee)->
  {Bee,MAC} = lists:nth(1,mpn_service:get_registered_bee(Bee)),
  {_, Bee,_,NextHop} = lists:nth(1,mpn_service:get_distance_vector(Bee)),
  postman_service:publish( {3, NextHop,Bee,?MyId, MAC}),
  Send_time = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
  timer:sleep(1000),
  {_, Bee,_, _, LastActive, _} = lists:nth(1,mpn_service:get_mpn_for(Bee)),
  TimeDiff = calendar:datetime_to_gregorian_seconds(LastActive) - Send_time,
  if
  TimeDiff >= 0 -> TimeDiff >= 0;
    true ->
      timer:sleep(1000),
      {_, Bee,_, _, LastActive2, _} = lists:nth(1,mpn_service:get_mpn_for(Bee)),
      TimeDiff2 = calendar:datetime_to_gregorian_seconds(LastActive2) - Send_time,
      TimeDiff2 >= 0
  end.

request_supported_services()->
  ok.