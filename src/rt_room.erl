-module(rt_room).

%% API
-export([
    create/0,
    stop/1
]).

-record(rt_room, {
    supervisor :: pid(),
    buffer :: pid(),
    server :: pid()
}).

-opaque rt_room() :: #rt_room{}.

-export_type([rt_room/0]).

%%====================================================================
%% API
%%====================================================================

-spec create() -> {ok, rt_room()}.
create() ->
    {ok, Supervisor} = rt_room_sup:create_room(),
    {ok, Buffer} = rt_room_inst_sup:get_buffer(Supervisor),
    {ok, Server} = rt_room_inst_sup:get_server(Supervisor),
    {ok, #rt_room{
        supervisor = Supervisor,
        buffer = Buffer,
        server = Server
    }}.

stop(Pid) ->
    gen_server:stop(Pid).
