-module(rt_room_register).

-behaviour(gen_server).

%% API
-export([
    start_link/0,
    get_room/1,
    create_room/1,
    delete_room/1,
    stop/0
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

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_room(Name) ->
    gen_server:call(?MODULE, {get_room, Name}).

create_room(Name) ->
    gen_server:call(?MODULE, {create_room, Name}).

delete_room(Name) ->
    gen_server:cast(?MODULE, {delete_room, Name}).

stop() ->
    gen_server:stop(?MODULE).

%%====================================================================
%% gen_server callbacks
%%====================================================================

terminate(_Reason, _Data) ->
    ok.

code_change(_Vsn, State, Data, _Extra) ->
    {ok, State, Data}.

init([]) ->
    {ok, #{}}.

handle_cast({delete_room, Name}, Data) ->
    {noreply, maps:without([Name], Data)};
handle_cast(EventContent, Data) ->
    print_unhandled_event(cast, EventContent, Data),
    {noreply, Data}.

handle_call({get_room, Name}, _From, Data) ->
    case Data of
        #{Name := Room} ->
            {reply, {ok, Room}, Data};
        _ ->
            {reply, {error, not_created}, Data}
    end;
handle_call({create_room, Name}, _From, Data) ->
    case Data of
        #{Name := _Room} ->
            {reply, {error, already_created}, Data};
        _ ->
            {ok, Room} = rt_room:create(),
            {reply, {ok, Room}, Data#{Name => Room}}
    end;
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
