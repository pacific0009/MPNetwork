%%%-------------------------------------------------------------------
%%% @author anand.ratna
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Sep 2019 8:33 PM
%%%-------------------------------------------------------------------
-module(postman_service).
-author("anand.ratna").

-behaviour(gen_server).

%% API
-export([start_link/0, stop/0, subscribe/1, unsubscribe/1, publish/1]).

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

start_link()->
  gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

stop()->
  gen_server:cast({global, ?MODULE}, stop).

subscribe(Subscriber)->
  gen_server:call({global, ?MODULE}, {subscribe, Subscriber}).

unsubscribe(Subscriber)->
  gen_server:call({global, ?MODULE}, {unsubscribe, Subscriber}).

publish(Message)->
  gen_server:call({global, ?MODULE}, {publish, Message}).
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
  postman:start(),
  {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------

handle_call({subscribe, Subscriber}, _From, State) ->
  {reply, postman:subscribe(Subscriber), State};

handle_call({unsubscribe, Subscriber}, _From, State) ->
  {reply, postman:unsubscribe(Subscriber), State};

handle_call({publish, Message}, _From, State) ->
  {reply, postman:publish(Message), State}.
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

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
