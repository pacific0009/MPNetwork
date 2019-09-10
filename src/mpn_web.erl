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


dasboard_content() ->
  MainHeader = "<!DOCTYPE html>
  <html>
  <head>
<style>
table {
	border-collapse: collapse;
	margin:150px, 50px, 50px, 50px;
	float: left;
	width:30%;
	padding:150px, 50px, 50px, 50px;
	}

/* Zebra striping */
tr:nth-of-type(odd) {
	background: #eee;
	}

th {
	background: #3498db;
	color: white;
	font-weight: bold;
	}

td, th {
	padding: 10px;
	border: 1px solid #ccc;
	text-align: left;
	font-size: 18px;
	}

/*
Max width before this PARTICULAR table gets nasty
This query will take effect for any screen smaller than 760px
and also iPads specifically.
*/
@media
only screen and (max-width: 760px),
(min-device-width: 768px) and (max-device-width: 1024px)  {

	table {
	  	float: left;
	  	width:30%;
	  	margin:50px, 50px, 50px, 50px;
	}

	/* Force table to not be like tables anymore */
	table, thead, tbody, th, td, tr {
		display: block;
	}

	/* Hide table headers (but not display: none;, for accessibility) */
	thead tr {
		position: absolute;
		top: -9999px;
		left: -9999px;
	}

	tr { border: 1px solid #ccc; }

	td {
		border: none;
		border-bottom: 1px solid #eee;
		position: relative;
		padding-left: 50%;
	}

	td:before {
		/* Now like a table header */
		position: absolute;
		/* Top/left values mimic padding */
		top: 6px;
		left: 6px;
		width: 45%;
		padding-right: 10px;
		white-space: nowrap;
		/* Label the data */
		content: attr(data-column);

		color: #000;
		font-weight: bold;
	}

}

</style>
</head>",
  Header = "<h3>MPNetwork Dashboard<h3> Welcomes!",
  MTHead ="<table><caption>MPNetwork Bee Nodes</caption><thead><tr><th>Bee</th><th>Available</th><th>MAC</th><th>Last Active</th><th>Alive</th></tr></thead>",
  MT = string:join(lists:map(fun format_MT/1, mpn_service:get_mpn_table()), " "),
  RTHead ="<table><caption>MPNetwork Routing Table</caption><thead><tr><th>Destination</th><th>Distance</th><th>NextHop</th></tr></thead>",
  RT = string:join(lists:map(fun format_RT/1, mpn_service:get_routing_table()), " "),
  CT = "</table>",
  string:join([MainHeader, Header,"<div>",MTHead,MT,CT,RTHead,RT,CT,"</div>"]," ").

