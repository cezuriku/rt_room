-module(rt_room_inst_buffer).

-behaviour(gen_server).

%% API
-export([
    start_link/0,
    add_player/2,
    remove_player/2,
    move_player/4,
    get_frame/2,
    stop/1
]).

%% gen_server callback
-export([
    terminate/2,
    code_change/4,
    init/1,
    handle_cast/2,
    handle_call/3,
    handle_info/2
]).

-type frame() :: non_neg_integer().

-record(data, {
    players = #{} :: #{pid() := rt_room:player_id()},
    next_player_id = 0 :: rt_room:player_id(),
    new_players = #{} :: rt_room:players(),
    players_positions = #{} :: #{frame() := rt_room:players()},
    removed_players = [] :: [non_neg_integer()],
    frame = 1 :: frame()
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link(?MODULE, [], []).

-spec add_player(BufferPid :: pid(), PlayerPid :: pid()) -> {ok, rt_room:player_id()}.
add_player(BufferPid, PlayerPid) ->
    gen_server:call(BufferPid, {add_player, PlayerPid}).

-spec remove_player(BufferPid :: pid(), PlayerPid :: pid()) -> ok.
remove_player(BufferPid, PlayerPid) ->
    gen_server:cast(BufferPid, {remove_player, PlayerPid}).

-spec move_player(
    Pid :: pid(),
    PlayerId :: rt_room:player_id(),
    Frame :: non_neg_integer(),
    Position :: {integer(), integer()}
) -> ok | {error, term()}.
move_player(Pid, PlayerId, Frame, Position) ->
    gen_server:call(Pid, {move_player, PlayerId, Frame, Position}).

-spec get_frame(BufferPid :: pid(), Frame :: frame()) ->
    {
        AddedPlayers :: rt_room:players(),
        UpdatedPlayers :: rt_room:players(),
        DeletedPlayers :: [rt_room:player_id()]
    }.
get_frame(BufferPid, Frame) ->
    gen_server:call(BufferPid, {get_frame, Frame}).

stop(Pid) ->
    gen_server:stop(Pid).

%%====================================================================
%% gen_server callbacks
%%====================================================================

terminate(_Reason, _Data) ->
    ok.

code_change(_Vsn, State, Data, _Extra) ->
    {ok, State, Data}.

init([]) ->
    {ok, #data{}}.

handle_cast(
    {remove_player, PlayerPid},
    #data{removed_players = RemovedPlayers, players = Players} = Data
) ->
    #{PlayerPid := PlayerId} = Players,
    {noreply, Data#data{
        removed_players = [PlayerId | RemovedPlayers],
        players = maps:without([PlayerPid], Players)
    }};
handle_cast(EventContent, Data) ->
    print_unhandled_event(cast, EventContent, Data),
    {noreply, Data}.

handle_call(
    {add_player, PlayerPid},
    _From,
    #data{players = Players, new_players = NewPlayers, next_player_id = NextId} = Data
) ->
    {reply, {ok, NextId}, Data#data{
        players = Players#{PlayerPid => NextId},
        new_players = NewPlayers#{NextId => _Position = {0, 0}},
        next_player_id = NextId + 1
    }};
handle_call(
    {move_player, PlayerId, ReqFrame, Position},
    _From,
    #data{players_positions = Positions, frame = Frame} = Data
) ->
    if
        ReqFrame =< Frame ->
            {reply, {error, frame_too_late}, Data};
        true ->
            FramePositions =
                case Positions of
                    #{ReqFrame := FramePositions0} ->
                        FramePositions0#{PlayerId => Position};
                    _ ->
                        #{PlayerId => Position}
                end,
            {reply, ok, Data#data{
                players_positions = Positions#{ReqFrame => FramePositions}
            }}
    end;
handle_call(
    {get_frame, ReqFrame},
    _From,
    #data{
        new_players = AddedPlayers,
        players_positions = Positions,
        removed_players = RemovedPlayers
    } = Data
) ->
    FramePositions =
        case Positions of
            #{ReqFrame := FramePositions0} ->
                FramePositions0;
            _ ->
                #{}
        end,
    {reply, {AddedPlayers, FramePositions, RemovedPlayers}, Data#data{
        frame = ReqFrame,
        new_players = #{},
        players_positions = maps:without([ReqFrame], Positions),
        removed_players = []
    }};
handle_call(EventContent, _From, Data) ->
    print_unhandled_event(call, EventContent, Data),
    {reply,
        {error,
            {unhandled_event, #{
                event => EventContent,
                data => Data
            }}},
        Data}.

handle_info(EventContent, Data) ->
    print_unhandled_event(info, EventContent, Data),
    {noreply, Data}.

%%====================================================================
%% Internal functions
%%====================================================================

print_unhandled_event(Type, Content, Data) ->
    io:format(
        "Unhandled event:~n~p~n",
        [
            #{
                event_type => Type,
                event_content => Content,
                data => Data
            }
        ]
    ).
