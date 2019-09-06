%%%-------------------------------------------------------------------
%%% @author anand.ratna
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Aug 2019 5:49 PM
%%%-------------------------------------------------------------------
-module(mpn).
-author("anand.ratna").
-include_lib("stdlib/include/qlc.hrl").
-include("config.hrl").
%% API
-export([start/0, get_mpn_table/0,update_routing_table/3, get_distance_vector/1,
  get_routing_table/0, init_mpn/2, register_bee/1, unregister_bee/1,
  get_registered_bee/1, get_next_hop_bees/0, alive_allocate_mpn_id/2,
  set_bee_as_lost/1, get_mpn_for/1]).
-record(mpNetwork, {mpnId,available, mac, lastActive, alive}).
-record(mpnRoutingTable, {destination, distance, nextHop}).
-record(sequence, {number}).

start()->
  io:format("\t- Starting MPNetwork ["),
  initDB(),
  init_mpn(?MAX_BEES, ?MIN_SEQ),
  io:format("##]~n"),
  io:format("\t+ MPNetwork Started!~n"),
  register_bee(?MyMAC),
  insert_distance_vector(?MyId, 0, ?MyId),
  ok.

initDB() ->
  mnesia:create_schema([node()]),
  mnesia:start(),
  lists:foreach(fun(Item) -> timer:sleep(100), io:format("####")
                end, lists:seq(0,5)),
  try
    mnesia:table_info(type, mpNetwork),
    mnesia:table_info(type, mpnRoutingTable),
    mnesia:table_info(type, sequence)
  catch
    exit: _ ->
      mnesia:create_table(mpNetwork, [{attributes, record_info(fields, mpNetwork)},
        {type, bag}]),
      mnesia:create_table(mpnRoutingTable, [{attributes, record_info(fields, mpnRoutingTable)},
        {type, bag}]),
      mnesia:create_table(sequence, [{attributes, record_info(fields, sequence)},
        {type, bag}])
  end.

init_sequence(MINSeq)->
  F = fun() ->
    mnesia:write(#sequence{number = MINSeq})
      end,
  mnesia:transaction(F),
  ok.

init_mpn(MAX, MINSeq) ->
  mnesia:clear_table(mpNetwork),
  mnesia:clear_table(sequence),
  init_mpn_tbl(0,MAX),
  init_sequence(MINSeq).


init_mpn_tbl(Index, Max)->
  if
    Index == Max ->
      ok;
    true ->
      if
        Index < Max ->
          F = fun() ->
            mnesia:write(#mpNetwork{mpnId=Index, available = true})
              end,
          mnesia:transaction(F),
          init_mpn_tbl(Index+1, Max);
        true ->
          {failed, exceeded_max_nodes }
      end
  end.

get_mpn_table() ->
  % Return all record of mpNetwork
  AF = fun()->
    Query = qlc:q([X || X <- mnesia:table(mpNetwork)]),
    qlc:e(Query)
       end,
  {atomic, Results} = mnesia:transaction(AF),
  lists:sort(Results).

get_mpn_for(ID)->
  % Return single record of mpNetwork
  AF = fun()->
    Query = qlc:q([X || X <- mnesia:table(mpNetwork), X#mpNetwork.mpnId =:= ID]),
    qlc:e(Query)
       end,
  {atomic, Results} = mnesia:transaction(AF),
  Results.


get_distance_vector(ID) ->
  % Return distance vector record of routing table for destination ID
  AF = fun()->
    Query = qlc:q([X || X <- mnesia:table(mpnRoutingTable), X#mpnRoutingTable.destination =:= ID]),
    qlc:e(Query)
       end,
  {atomic, Results} = mnesia:transaction(AF),
  Results.

get_next_hop_bees() ->
  % List of all next bees
  AF = fun()->
    Query = qlc:q([X || X <- mnesia:table(mpnRoutingTable), X#mpnRoutingTable.nextHop =/= ?INFINITE]),
    Results = qlc:e(Query),
    lists:map(fun(Item)->Item#mpnRoutingTable.nextHop end, Results)
       end,
  {atomic, Results} = mnesia:transaction(AF),
  Results.

get_routing_table() ->
  % Return all records of routing table
  AF = fun()->
    Query = qlc:q([X || X <- mnesia:table(mpnRoutingTable)]),
    qlc:e(Query)
       end,
  {atomic, Results} = mnesia:transaction(AF),
  Results.

update_routing_table(ID, Distance, NextHop) ->
  if
    ID == ?MyId->
      Changed = false,
      io:format("\t\tPacket Ignored~n" );
    true ->
      DVs= get_distance_vector(ID),
      if
        length(DVs) == 0 ->
          Changed = true,
          insert_distance_vector(ID, Distance+1, NextHop);
        true ->
          {_, _, Dist, NH} = lists:nth(1, DVs),
          if
            Distance + 1 < Dist  ->
              Changed = true,
              insert_distance_vector(ID, Distance + 1, NextHop) ;
            true ->
              if
                NextHop == NH ->
                  if
                    Dist >= ?INFINITE->
                      Changed = true,
                      insert_distance_vector(ID, ?INFINITE, ?INFINITE) ;
                    true ->
                      if
                        Dist == Distance + 1 ->
                          Changed = false;
                        true ->
                          Changed = true,
                          insert_distance_vector(ID, Distance + 1, NextHop)
                      end
                  end;
                true ->
                  Changed = false,
                  io:format("\t\tPacket Ignored~n" )
              end
          end
      end
  end,
  Changed.


insert_distance_vector(ID, Distance, NextHop)  ->
  F = fun() ->
    mnesia:delete(mpnRoutingTable, ID, write),
    mnesia:write(#mpnRoutingTable{destination = ID, distance = Distance, nextHop = NextHop})
      end,
  mnesia:transaction(F),
  ok.

alive_allocate_mpn_id(MAC, ID) ->
  F = fun() ->
    mnesia:delete(mpNetwork, ID, write),
    mnesia:write(#mpNetwork{mpnId=ID, mac = MAC, available = false, alive = true, lastActive = calendar:universal_time()})
      end,
  mnesia:transaction(F),
  ID.

registered_bee(MAC) ->
  AF = fun()->
    Query = qlc:q([X || X <- mnesia:table(mpNetwork),
      X#mpNetwork.mac =:= MAC]),
    Results = qlc:e(Query),
    lists:map(fun(Item)->Item#mpNetwork.mpnId end, Results)
       end,
  {atomic, ID} = mnesia:transaction(AF),
  ID.

get_registered_bee(Id) ->
  AF = fun()->
    Query = qlc:q([X || X <- mnesia:table(mpNetwork),
      X#mpNetwork.mpnId =:= Id]),
    Results = qlc:e(Query),
    lists:map(fun(Item)->{Id, Item#mpNetwork.mac} end, Results)
       end,
  {atomic, ID} = mnesia:transaction(AF),
  ID.

register_new_bee(MAC) ->
  io:format("\t\tNew Bee <~p> Registered.~n",[MAC]),
  AF = fun()->
    Query = qlc:q([X || X <- mnesia:table(mpNetwork),
      X#mpNetwork.available =:= true]),
    Results = qlc:e(Query),
    lists:map(fun(Item)->Item#mpNetwork.mpnId end, Results)
       end,
  {atomic, Results} = mnesia:transaction(AF),
  AvailIds = lists:sort(Results),
  if
    length(AvailIds) == 0->
      MpnID = -1;
    true ->
      MpnID = alive_allocate_mpn_id(MAC, lists:nth(1, AvailIds))
  end,
  MpnID.

unregister_bee(ID) ->
  F = fun() ->
    mnesia:delete(mpNetwork, ID, write),
    mnesia:write(#mpNetwork{mpnId=ID, available = true})
      end,
  mnesia:transaction(F),
  ok.

register_bee(MAC) ->
  ID = registered_bee(MAC),
  if
    length(ID) > 0 ->
      MP=lists:nth(1,ID),
      {ok, MAC, MP};
    true ->
      MP = register_new_bee(MAC),
      if
        MP == -1->
          {failed, MAC, mpnid_unavailable};
        true ->
          {ok, MAC, MP}
      end
  end.

set_bee_as_lost(Bee)->
  io:format("\t\tBee~p is lost~n",[Bee]),
  {_, Bee,_, MAC, LastActive, _} = lists:nth(1,get_mpn_for(Bee)),
  F = fun() ->
    mnesia:delete(mpNetwork, Bee, write),
    mnesia:write(#mpNetwork{mpnId=Bee, mac = MAC, available = false, alive = false, lastActive = LastActive})
      end,
  mnesia:transaction(F),
  %% Todo: Set unreachable bees in routing table
  ok.
