%%%-------------------------------------------------------------------
%% @doc rt_room_app_sup top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(rt_room_app_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_all,
        intensity => 0,
        period => 1
    },
    RtRoomSup = #{
        id => rt_room_sup,
        start => {rt_room_sup, start_link, []},
        type => supervisor
    },
    Register = #{
        id => rt_room_register,
        start => {rt_room_register, start_link, []}
    },
    ChildSpecs = [RtRoomSup, Register],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
