%%%-------------------------------------------------------------------
%% @doc rt_room_inst top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(rt_room_inst_sup).

-behaviour(supervisor).

-export([start_link/0, get_buffer/1, get_server/1]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    {ok, Supervisor} = supervisor:start_link(?MODULE, []),
    {ok, Buffer} = get_buffer(Supervisor),
    {ok, Server} = get_server(Supervisor),
    ok = rt_room_inst_server:set_buffer(Server, Buffer),
    {ok, Supervisor}.

get_buffer(Supervisor) ->
    get_child(Supervisor, buffer).

get_server(Supervisor) ->
    get_child(Supervisor, server).

init([]) ->
    SupFlags = #{
        strategy => one_for_all,
        intensity => 0,
        period => 1
    },
    Buffer = #{
        id => buffer,
        start => {rt_room_inst_buffer, start_link, []}
    },
    Server = #{
        id => server,
        start => {rt_room_inst_server, start_link, []}
    },
    ChildSpecs = [Buffer, Server],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions

get_child(Supervisor, Type) ->
    Children = supervisor:which_children(Supervisor),
    {Type, Child, _, _} = lists:keyfind(Type, 1, Children),
    {ok, Child}.
