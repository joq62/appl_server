%%%-------------------------------------------------------------------
%% @doc appl_server public API
%% @end
%%%-------------------------------------------------------------------

-module(appl_server_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    appl_server_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
