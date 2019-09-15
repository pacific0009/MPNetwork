%%%-------------------------------------------------------------------
%%% @author anand.ratna
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Sep 2019 12:52 PM
%%%-------------------------------------------------------------------
-module(mpn_web).
-author("anand.ratna").
%% API
-export([start/0,index/3, home/3, dashboard/3]).

start() ->
  inets:start(),
  Pid = inets:start(httpd, [
    {modules, [
      mod_alias,
      mod_auth,
      mod_esi,
      mod_actions,
      mod_cgi,
      mod_dir,
      mod_get,
      mod_head,
      mod_log,
      mod_disk_log
    ]},
    {port, 8081},
    {server_name,"httpd_test"},
    {server_root,"www/tmp"},
    {document_root,"www/tmp/htdocs"},
    {bind_address, "localhost"},
    {erl_script_alias, {"/web", [mpn_web]}}]), io:fwrite("~p",[Pid]).

index(SessionID, _Env, _Input) -> mod_esi:deliver(SessionID, [
  "Content-Type: text/html\r\n\r\n", "<html><body>MpNetworks, Welcomes You!</body></html>" ]).

home(SessionID, _Env, _Input) -> mod_esi:deliver(SessionID, [
  "Content-Type: text/html\r\n\r\n", "<html><body>MpNetworks, Welcomes You!<hr>Every Thing set for you</body></html>" ]).

dashboard(SessionID, _Env, _Input) -> mod_esi:deliver(SessionID, [
  "Content-Type: text/html\r\n\r\n",dasboard_content()]).


format_MT({mpNetwork, Bee, Available, Mac, Time, Alive}) ->
  lists:flatten(io_lib:format("<tr><td>~p</td><td>~p</td><td>~p</td><td>~p</td><td>~p</td></tr>",
    [Bee, Available, mpn_utils:b2l(Mac),mpn_utils:to_timestamp(Time), Alive])).

format_RT({mpnRoutingTable, Destination, Distance, NextHop}) ->
  lists:flatten(io_lib:format("<tr><td>~p</td><td>~p</td><td>~p</td></tr>",
    [Destination, Distance, NextHop])).

format_BS({mpnBeesState, Mac, Service, Data, Timestamp}) ->
  lists:flatten(io_lib:format("<tr><td>~p</td><td>~p</td><td>~p</td><td>~p</td><td></tr>",
    [Mac, Service, Data, mpn_utils:to_timestamp(Timestamp)])).

format_RS({mpnBeesServices, Mac, Service1, Service2, Service3, Service4}) ->
  lists:flatten(io_lib:format("<tr><td>~p</td><td>~p</td><td>~p</td><td>~p</td><td>~p</td></tr>",
    [Mac, Service1, Service2, Service3, Service4])).

dasboard_content() ->
  Style = "<style>@import url(https://fonts.googleapis.com/css?family=Open+Sans:400,600);

*, *:before, *:after {
  margin: 0;
  padding: 0;
  box-sizing: border-box;
}

body {
  background: #105469;
  font-family: 'Open Sans', sans-serif;
}
table {
  background: #012B39;
  border-radius: 0.25em;
  border-collapse: collapse;
  margin: 1em;
}
th {
  border-bottom: 1px solid #364043;
  color: #E2B842;
  font-size: 0.85em;
  font-weight: 600;
  padding: 0.5em 1em;
  text-align: left;
}
td {
  color: #fff;
  font-weight: 400;
  padding: 0.65em 1em;
}
.disabled td {
  color: #4F5F64;
}
tbody tr {
  transition: background 0.25s ease;
}
tbody tr:hover {
  background: #014055;
}

caption{
  color: #fff;
  font-size: 1.2em;
  font-weight: 600;
}</style>",
  MainHeader = "<!DOCTYPE html>
<html lang=\"en\">
  <head>
    <meta charset=\"utf-8\">
    <meta http-equiv=\"X-UA-Compatible\" content=\"IE=edge\">
    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
  ",
  Title = "<title> MpNetwork </title>",

  MainFooter=  "<!-- jQuery (necessary for Bootstrap's JavaScript plugins) -->
    <script src=\"https://ajax.googleapis.com/ajax/libs/jquery/1.12.4/jquery.min.js\"></script>
    <!-- Include all compiled plugins (below), or include individual files as needed -->
    <script src=\"js/bootstrap.min.js\"></script>
  </body>
</html>",
  Header = "<h3>MPNetwork Dashboard<h3> Welcomes!",
  MTHead ="<table class=\table table-striped\"><caption>MPNetwork Bee Nodes</caption><thead><tr><th>Bee</th><th>Available</th><th>MAC</th><th>Last Active</th><th>Alive</th></tr></thead>",
  MT = string:join(lists:map(fun format_MT/1, mpn_service:get_mpn_table()), " "),
  RTHead ="<table class=\table table-striped\"><caption>MPNetwork Routing Table</caption><thead ><tr><th>Destination</th><th>Distance</th><th>NextHop</th></tr></thead>",
  RT = string:join(lists:map(fun format_RT/1, mpn_service:get_routing_table()), " "),
  RSHead ="<table class=\table table-striped\"><caption>MPNetwork Registered Services</caption><thead ><tr><th>Mac</th><th>Service 1</th><th>Service 2</th><th>Service 3</th><th>Service 4</th></tr></thead>",
  RS = string:join(lists:map(fun format_RS/1, mpn_service:get_registered_services()), " "),
  BSHead ="<table class=\table table-striped\"><caption>MPNetwork Services State</caption><thead ><tr><th>Mac</th><th>Service</th><th>Data</th><th>Timestamp</th></tr></thead>",
  BS = string:join(lists:map(fun format_BS/1, mpn_service:get_bees_state()), " "),
  CT = "</table>",
  string:join([MainHeader,Style,"</head>",Title, "<body>", Header,"<div class=\"table-responsive\">",MTHead,MT,CT,RTHead,RT,CT,RSHead,RS,CT,BSHead,BS,CT,"</div>", MainFooter]," ").

