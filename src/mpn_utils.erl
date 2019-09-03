%%%-------------------------------------------------------------------
%%% @author anand.ratna
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Sep 2019 2:11 PM
%%%-------------------------------------------------------------------
-module(mpn_utils).
-author("anand.ratna").

%% API
-export([mpn_debug/1]).

mpn_debug(Y) ->
  [io:format("*mpn: ~s ~n",X) || X <- Y],
  ok.
