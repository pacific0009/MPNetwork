%%%-------------------------------------------------------------------
%%% @author anand.ratna
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Sep 2019 11:04 AM
%%%-------------------------------------------------------------------
-module(mpNetwork).
-author("anand.ratna").

-behaviour(application).

%% Application callbacks
-export([start/2,
  stop/1, start/0, stop/0]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @end
%%--------------------------------------------------------------------
start()->
  application:start(?MODULE).

-spec(start(StartType :: normal | {takeover, node()} | {failover, node()},
    StartArgs :: term()) ->
  {ok, pid()} |
  {ok, pid(), State :: term()} |
  {error, Reason :: term()}).
start(_StartType, _StartArgs) ->
  case mpn_supervisor:start_link() of
    {ok, Pid} ->
      mpn_web:start(),
      {ok, Pid};
    Error ->
      Error
  end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
stop()->
  application:stop(?MODULE).

-spec(stop(State :: term()) -> term()).
stop(_State) ->
  inets:stop(),
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
