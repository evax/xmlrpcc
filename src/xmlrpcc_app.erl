-module(xmlrpcc_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    xmlrpcc_sup:start_link().

stop(_State) ->
    ok.
