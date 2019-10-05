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
-export([start/0, on_receive_packet/0, prune_lost_bees/0, advertise_routing_table/0, bee_sting/1]).
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
  postman_service:publish({0, 2, ?MAX_BEES,S, ?MyId, Data}),
  timer:sleep(2000),
  postman_service:publish({0, 5, BeeId,S, ?MyId, DATA}),
  advertise_routing_table(),
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
  Response = list_to_integer(binary_to_list(string:slice(DATA,4,4)),16),
  %% mpn_service:store_response(MAC, Service, Response),
  mpn_service:record_state(MAC, Service, Response).

on_receive_packet()->
  receive
    {_,0,_,_,S,DATA} ->
      handle_routing_packet( S,DATA),
      on_receive_packet();
    {_,1,_,_,S,DATA} ->
      handle_bee_registration( S,DATA),
      on_receive_packet();
    {_,2,_,_,S,DATA} ->
      %% ignore
      on_receive_packet();
    {_,3,_,_,S,DATA} ->
      %% ignore
      on_receive_packet();
    {_,4,_,?MyId,S,DATA}->
      update_bee_alive(S,DATA),
      on_receive_packet();
    {_,5,_,_,S,DATA} ->
      %% ignore
      on_receive_packet();
    {_,6,_,?MyId,S,DATA}->
      register_services(S, DATA),
      on_receive_packet();
    {_,_,_,?MyId,S,DATA}->
      store_response(S, DATA),
      on_receive_packet();
    Other->
      io:format("$failed ~p~n", [Other]),
      on_receive_packet()
  end.


prune_lost_bees() ->
  %Next_hop_bees = lists:usort(mpn_service:get_next_hop_bees()),
  MPN = mpn_service:get_mpn_table(),
  RegBees = lists:filtermap(fun({_,Bee,Avail,_,_,_}) -> case Avail of false -> {true, Bee}; _ -> false end end, MPN),

  lists:foreach(fun(Bee) ->
    erlang:spawn(node(),mpn_controller,bee_sting,[Bee])
                end, RegBees),
  ok.


bee_sting(Bee)->
  BHeart = [{field1, unicode:characters_to_binary("♥")}],
  [{field1, Heart}|_] = BHeart,
  if
    Bee == ?MyId -> io:format("\tBee(~p) ~ts~n",[Bee, Heart]);
    true ->
      Send_time = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
      Sting = is_bee_sting(0, 10, Bee, Send_time),
      if
        Sting =:= true -> io:format("Bee(~p) ~ts~n",[Bee, Heart]);
        true ->
          mpn_service:set_bee_as_lost(Bee)
      end
  end.

is_bee_sting(Index, Max, Bee, Send_time)->
  {Bee,MAC} = lists:nth(1,mpn_service:get_registered_bee(Bee)),
  {_, Bee,_,NextHop} = lists:nth(1,mpn_service:get_distance_vector(Bee)),
  postman_service:publish( {0, 3, NextHop,Bee,?MyId, MAC}),
  timer:sleep(5000),
  {_, Bee,_, _, LastActive, _} = lists:nth(1,mpn_service:get_mpn_for(Bee)),
  TD = calendar:datetime_to_gregorian_seconds(LastActive) - Send_time,
  if
      TD >= 0 ->
      true;
    true ->
      if
        Index < Max ->
          is_bee_sting(Index+1, Max, Bee, Send_time);
        true ->
          false
      end
  end.


advertise_routing_table()->
  RT = mpn_service:get_routing_table(),
  DV_2 = round(length(RT)/2)-1,
  lists:foreach(fun(Count) ->
    C1 = Count*2+1,
    C2 = Count*2+2,
    {_,P1,D1,_} = lists:nth(C1, RT),
    {_,P2,D2,_} = lists:nth(C2, RT),
    DN1 = string:pad(integer_to_list(P1, 16), 2, leading, "0"),
    DT1 = string:pad(integer_to_list(D1, 16), 2, leading, "0"),
    DN2 = string:pad(integer_to_list(P2, 16), 2, leading, "0"),
    DT2 = string:pad(integer_to_list(D2, 16), 2, leading, "0"),
    Buff = lists:flatten(io_lib:format("~s~s~s~s", [list_to_binary(DN1),
      list_to_binary(DT1),
      list_to_binary(DN2),
      list_to_binary(DT2)])),
    Data = list_to_binary(Buff),
    postman_service:publish({0, 0, ?INFINITE,?INFINITE, ?MyId, Data})
                end, lists:seq(0,DV_2)),
  ok.

%%advertise_lost_node()->
%%  MPN = mpn:get_mpn_table(),
%%  LiveNodes = lists:filtermap(fun({Bee,0,0,0,Live}) -> case Live of true -> {true, Bee}; _ -> false end end,
%%    MPN),
%%
%%  ok.
