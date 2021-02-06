-module(rt_room_inst_server).

-behaviour(gen_server).

%% API
-export([
    start_link/0,
    set_buffer/2,
    add_observer/3,
    remove_observer/2,
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

-record(data, {
    buffer :: pid() | undefined,
    observers = [] :: [{module(), pid()}],
    players = #{} :: rt_room:players(),
    next_frame :: non_neg_integer()
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link(?MODULE, [], []).

set_buffer(Pid, Buffer) ->
    gen_server:call(Pid, {set_buffer, Buffer}).

-spec add_observer(pid(), module(), pid()) -> rt_room:players().
add_observer(Pid, ObserverModule, ObserverPid) ->
    gen_server:call(Pid, {add_observer, ObserverModule, ObserverPid}).

remove_observer(Pid, ObserverPid) ->
    gen_server:cast(Pid, {remove_observer, ObserverPid}).

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
    timer:send_interval(100, tick),
    {ok, #data{
        next_frame = 2
    }}.

handle_cast({remove_observer, Observer}, #data{observers = Observers} = Data) ->
    {noreply, Data#data{observers = lists:keydelete(Observer, 2, Observers)}};
handle_cast(EventContent, Data) ->
    print_unhandled_event(cast, EventContent, Data),
    {noreply, Data}.

handle_call({set_buffer, Buffer}, _From, Data) ->
    {reply, ok, Data#data{buffer = Buffer}};
handle_call(
    {add_observer, ObserverModule, ObserverPid},
    _From,
    #data{
        observers = Observers,
        players = Players
    } = Data
) ->
    {reply, Players, Data#data{observers = [{ObserverModule, ObserverPid} | Observers]}};
handle_call(EventContent, _From, Data) ->
    print_unhandled_event(call, EventContent, Data),
    {reply,
        {error,
            {unhandled_event, #{
                event => EventContent,
                data => Data
            }}},
        Data}.

handle_info(
    tick,
    #data{
        buffer = Buffer,
        observers = Observers,
        players = Players,
        next_frame = Frame
    } = Data
) ->
    {AddedPlayers, UpdatedPlayers, DeletedPlayers} =
        rt_room_inst_buffer:get_frame(Buffer, Frame),
    NewPlayers = maps:without(DeletedPlayers, maps:merge(Players, UpdatedPlayers)),
    lists:foreach(
        fun({Module, Pid}) ->
            ok = Module:handle_new_frame(
                Pid,
                Frame,
                AddedPlayers,
                NewPlayers,
                DeletedPlayers
            )
        end,
        Observers
    ),
    {noreply, Data#data{
        next_frame = Frame + 1,
        players = maps:without(DeletedPlayers, maps:merge(NewPlayers, AddedPlayers))
    }};
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
