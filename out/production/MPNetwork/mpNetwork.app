%%%-------------------------------------------------------------------
%%% @author anand.ratna
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Sep 2019 11:11 AM
%%%-------------------------------------------------------------------
{application, mpNetwork, [
  {description, "Mesh protocol network system."},
  {vsn, "1"},
  {registered, []},
  {applications, [
    kernel,
    stdlib
  ]},
  {mod, {mpNetwork, []}},
  {env, []}
]}.