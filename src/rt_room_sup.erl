%%%-------------------------------------------------------------------
%% @doc rt_room top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(rt_room_sup).

-behaviour(supervisor).

-export([start_link/0, create_room/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    {ok, _Pid} = supervisor:start_link({local, ?SERVER}, ?MODULE, []).

create_room() ->
    supervisor:start_child(?SERVER, []).

init([]) ->
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => 0,
        period => 1
    },
    Inst = #{
        id => instance,
        start => {rt_room_inst_sup, start_link, []},
        restart => temporary
    },
    ChildSpecs = [Inst],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
