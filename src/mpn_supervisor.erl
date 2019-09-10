%%%-------------------------------------------------------------------
%%% @author anand.ratna
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Sep 2019 9:54 PM
%%%-------------------------------------------------------------------
-module(mpn_supervisor).
-author("anand.ratna").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------

start_link() ->
  supervisor:start_link({global, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------

init([]) ->
  io:format("~p (~p) Starting..~n", [{global, ?MODULE}, self()]),
  RestartStrategy = {rest_for_one, 1000, 5},
  Mpn = {mpn_service_id, {mpn_service, start_link, []},
    permanent, infinity, worker, [mpn_service]},
  Postman = {postman_service_id, {postman_service, start_link, []},
    permanent, infinity, worker, [postman_service]},
  MpnController = {mpn_controller_service_id, {mpn_controller_service, start_link, []},
    permanent, infinity, worker, [mpn_controller_service]},
  Children = [Mpn, Postman, MpnController],
  {ok, {RestartStrategy, Children}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
