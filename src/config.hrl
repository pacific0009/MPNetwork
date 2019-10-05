%%%-------------------------------------------------------------------
%%% @author anand.ratna
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Sep 2019 1:51 PM
%%%-------------------------------------------------------------------
-author("anand.ratna").
-define(MyMAC, <<"AF:E1:01">>).
-define(MAX_BEES, 10).
-define(MyId, 0).
-define(SIZE_OF_DATA, 8).
-define(INFINITE, ?MAX_BEES).
-define(SERIAL_PORT, <<"/dev/cu.usbmodem14221">>).
-define(BAUD_RATE, 9600).
-define(PYPATH, "/Users/anand.ratna/IdeaProjects/MPNetork/src").
-define(INTERVAL, 6000). % One minute
-define(ART_INTERVAL, 12000). % One minute