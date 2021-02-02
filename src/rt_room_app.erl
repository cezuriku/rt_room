%%%-------------------------------------------------------------------
%% @doc rt_room public API
%% @end
%%%-------------------------------------------------------------------

-module(rt_room_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    rt_room_app_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
