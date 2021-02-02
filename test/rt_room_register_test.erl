-module(rt_room_register_test).

-include_lib("eunit/include/eunit.hrl").

start_register_test() ->
    rt_room_sup:start_link(),
    {ok, _Pid} = rt_room_register:start_link().

get_not_created_room_test() ->
    {error, not_created} = rt_room_register:get_room(test).

create_room_test() ->
    {ok, _Room} = rt_room_register:create_room(test).

already_created_room_test() ->
    {error, already_created} = rt_room_register:create_room(test).

get_created_room_test() ->
    {ok, _Room} = rt_room_register:get_room(test).

delete_room_test() ->
    {ok, _Room} = rt_room_register:get_room(test),
    ok = rt_room_register:delete_room(test),
    {error, not_created} = rt_room_register:get_room(test).
