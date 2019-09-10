%%%-------------------------------------------------------------------
%%% @author anand.ratna
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Sep 2019 8:18 PM
%%%-------------------------------------------------------------------
-module(mpn_service).
-author("anand.ratna").

-behaviour(gen_server).

%% API
-export([start_link/0, get_mpn_table/0,update_routing_table/3, get_distance_vector/1,
  get_routing_table/0, register_bee/1, unregister_bee/1,
  get_registered_bee/1, get_next_hop_bees/0, alive_allocate_mpn_id/2,
  set_bee_as_lost/1, get_mpn_for/1, get_mpn_table_json/0, get_routing_table_list/0,
  register_bee_services/5, store_response/3, store_request/3,
  get_registered_services_for/1, get_registered_services/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------

start_link() ->
  gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

get_mpn_table()->
  gen_server:call({global, ?MODULE}, {get_mpn_table}).

get_mpn_table_json()->
  gen_server:call({global, ?MODULE}, {get_mpn_table_json}).

get_routing_table_list()->
  gen_server:call({global, ?MODULE}, {get_routing_table_list}).
get_mpn_for(Bee)->
  gen_server:call({global, ?MODULE}, {get_mpn_for, Bee}).

update_routing_table(Destination, Distance, NextHop)->
  gen_server:call({global, ?MODULE}, {update_routing_table, Destination, Distance, NextHop}).

get_distance_vector(Destination)->
  gen_server:call({global, ?MODULE}, {get_distance_vector, Destination}).

get_routing_table()->
  gen_server:call({global, ?MODULE}, {get_routing_table}).

register_bee(MAC)->
  gen_server:call({global, ?MODULE}, {register_bee, MAC}).

unregister_bee(Bee)->
  gen_server:call({global, ?MODULE}, {unregister_bee, Bee}).

get_registered_bee(MAC)->
  gen_server:call({global, ?MODULE}, {get_registered_bee, MAC}).

get_next_hop_bees()->
  gen_server:call({global, ?MODULE}, {get_next_hop_bees}).

alive_allocate_mpn_id(MAC, ID)->
  gen_server:call({global, ?MODULE}, {alive_allocate_mpn_id, MAC, ID}).

set_bee_as_lost(Bee)->
  gen_server:call({global, ?MODULE}, {set_bee_as_lost, Bee}).

register_bee_services(Mac, S1, S2, S3, S4) ->
  gen_server:call({global, ?MODULE}, {register_bee_services, Mac, S1, S2, S3, S4}).

store_request(Mac, Service, Request) ->
  gen_server:call({global, ?MODULE}, {store_request, Mac, Service, Request}).

store_response(Mac, Service, Response) ->
  gen_server:call({global, ?MODULE}, {store_response, Mac, Service, Response}).
get_registered_services() ->
  gen_server:call({global, ?MODULE}, {get_registered_services}).
get_registered_services_for(Mac) ->
  gen_server:call({global, ?MODULE}, {get_registered_services_for, Mac}).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------

init([]) ->
  process_flag(trap_exit, true),
  io:format("~p (~p) Starting..~n", [{global, ?MODULE}, self()]),
  mpn:start(),
  {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
handle_call({set_bee_as_lost, Bee}, _From, State) ->
  {reply, mpn:set_bee_as_lost(Bee), State};

handle_call({alive_allocate_mpn_id, MAC, ID}, _From, State) ->
  {reply, mpn:alive_allocate_mpn_id(MAC, ID), State};

handle_call({get_next_hop_bees}, _From, State) ->
  {reply, mpn:get_next_hop_bees(), State};

handle_call({get_registered_bee, MAC}, _From, State) ->
  {reply, mpn:get_registered_bee(MAC), State};

handle_call({unregister_bee, Bee}, _From, State) ->
  {reply, mpn:unregister_bee(Bee), State};

handle_call({register_bee, MAC}, _From, State) ->
  {reply, mpn:register_bee(MAC), State};

handle_call({get_routing_table}, _From, State) ->
  {reply, mpn:get_routing_table(), State};

handle_call({get_distance_vector, Destination}, _From, State) ->
  {reply, mpn:get_distance_vector(Destination), State};

handle_call({update_routing_table, Destination, Distance, NextHop}, _From, State) ->
  {reply, mpn:update_routing_table(Destination,Distance,NextHop), State};

handle_call({get_mpn_for, Bee}, _From, State) ->
  {reply, mpn:get_mpn_for(Bee), State};

handle_call({get_mpn_table}, _From, State) ->
  {reply, mpn:get_mpn_table(), State};

handle_call({get_routing_table_list}, _From, State) ->
  {reply, mpn:get_routing_table_list(), State};

handle_call({register_bee_services, Mac, S1, S2, S3, S4}, _From, State) ->
  {reply, mpn:register_bee_services(Mac, S1, S2, S3, S4), State};

handle_call({store_response, Mac, Service, Response}, _From, State) ->
  {reply, mpn:store_response(Mac, Service, Response), State};

handle_call({store_request, Mac, Service, Request}, _From, State) ->
  {reply, mpn:store_request(Mac, Service, Request), State};

handle_call({get_registered_services}, _From, State) ->
  {reply, mpn:get_registered_services(), State};

handle_call({get_registered_services_for, Mac}, _From, State) ->
  {reply, mpn:get_registered_services_for(Mac), State};

handle_call({get_mpn_table_json}, _From, State) ->
  {reply, mpn:get_mpn_tableJson(), State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------

handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
