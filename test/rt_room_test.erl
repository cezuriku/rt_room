-module(rt_room_test).

-include_lib("eunit/include/eunit.hrl").

create_rt_room_test() ->
    application:ensure_all_started(rt_room),
    {ok, _RtRoom} = rt_room:create(),
    % {ok, PlayerId} = rt_room:add_player(RtRoom, self()),
    % ok = rt_room:move_player(RtRoom, PlayerId, 8, {500, 500}),
    ok.
