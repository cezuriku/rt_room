-module(rt_room).

%% API
-export([
    create/0,
    add_player/3,
    remove_player/2,
    move_player/4,
    stop/1
]).

-record(rt_room, {
    supervisor :: pid(),
    buffer :: pid(),
    server :: pid()
}).

-opaque rt_room() :: #rt_room{}.

-type player_id() :: non_neg_integer().
-type players() :: #{player_id() := Position :: {integer(), integer()}}.

-export_type([rt_room/0, players/0, player_id/0]).

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

-spec add_player(rt_room(), Module :: module(), PlayerPid :: pid()) -> {ok, player_id()}.
add_player(#rt_room{buffer = Buffer, server = Server}, Module, PlayerPid) ->
    rt_room_inst_server:add_observer(Server, Module, PlayerPid),
    rt_room_inst_buffer:add_player(Buffer, PlayerPid).

-spec remove_player(rt_room(), PlayerPid :: pid()) -> ok.
remove_player(#rt_room{buffer = Buffer, server = Server}, PlayerPid) ->
    rt_room_inst_server:remove_observer(Server, PlayerPid),
    rt_room_inst_buffer:remove_player(Buffer, PlayerPid).

-spec move_player(
    rt_room(),
    PlayerId :: player_id(),
    Frame :: non_neg_integer(),
    Position :: {integer(), integer()}
) -> ok.
move_player(#rt_room{buffer = Buffer}, PlayerId, Frame, Position) ->
    rt_room_inst_buffer:move_player(Buffer, PlayerId, Frame, Position).

stop(#rt_room{supervisor = RoomSup}) ->
    rt_room_sup:stop_room(RoomSup).
