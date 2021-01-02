-module(rt_room_inst_server).

-behaviour(gen_server).

%% API
-export([
    start_link/0,
    set_buffer/2,
    add_observer/2,
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
    buffer :: pid(),
    observers :: [pid()],
    players :: #{integer() := Position :: {integer(), integer()}}
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link(?MODULE, [], []).

set_buffer(Pid, Buffer) ->
    gen_server:call(Pid, {set_buffer, Buffer}).

add_observer(Pid, ObserverPid) ->
    gen_server:call(Pid, {add_observer, ObserverPid}).

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
    {ok, #data{
        observers = [],
        players = #{}
    }}.

handle_cast({remove_observer, Observer}, #data{observers = Observers} = Data) ->
    {noreply, Data#data{observers = lists:delete(Observer, Observers)}};
handle_cast(EventContent, Data) ->
    print_unhandled_event(cast, EventContent, Data),
    {noreply, Data}.

handle_call({set_buffer, Buffer}, _From, Data) ->
    {reply, ok, Data#data{buffer = Buffer}};
handle_call({add_observer, Observer}, _From, #data{observers = Observers} = Data) ->
    {reply, ok, Data#data{observers = [Observer | Observers]}};
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
