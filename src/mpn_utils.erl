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
-export([mpn_debug/1, to_timestamp/1, b2l/1]).

mpn_debug(Y) ->
  [io:format("*mpn: ~s ~n",X) || X <- Y],
  ok.
to_timestamp(Time) ->
  case Time of
    undefined -> undefined;
    {{Year,Month,Day},{Hours,Minutes,Seconds}} ->
      (calendar:datetime_to_gregorian_seconds(
        {{Year,Month,Day},{Hours,Minutes,Seconds}}
      ) - 62167219200);
    Other ->
      Other
  end.
b2l(Data)->
  case Data of
      undefined ->Data;
      Other->
        binary_to_list(Other)
end.