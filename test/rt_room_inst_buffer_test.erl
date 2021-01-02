-module(rt_room_inst_buffer_test).

-include_lib("eunit/include/eunit.hrl").

start_buffer_test() ->
    {ok, _Pid} = rt_room_inst_buffer:start_link().

add_player_test() ->
    {ok, Pid} = rt_room_inst_buffer:start_link(),
    {ok, 0} = rt_room_inst_buffer:add_player(Pid, self()).

check_added_player_test() ->
    {ok, Pid} = rt_room_inst_buffer:start_link(),
    {ok, Id} = rt_room_inst_buffer:add_player(Pid, self()),
    {#{Id := {0, 0}}, #{}, []} = rt_room_inst_buffer:get_frame(Pid, 2).

check_updated_player_test() ->
    {ok, Pid} = rt_room_inst_buffer:start_link(),
    {ok, Id} = rt_room_inst_buffer:add_player(Pid, self()),
    _ = rt_room_inst_buffer:get_frame(Pid, 2),
    rt_room_inst_buffer:move_player(Pid, Id, 3, {300, 300}),
    {_, #{Id := {300, 300}}, []} = rt_room_inst_buffer:get_frame(Pid, 3).

check_removed_player_test() ->
    {ok, Pid} = rt_room_inst_buffer:start_link(),
    {ok, Id} = rt_room_inst_buffer:add_player(Pid, self()),
    _ = rt_room_inst_buffer:get_frame(Pid, 2),
    rt_room_inst_buffer:remove_player(Pid, self()),
    {_, _, [Id]} = rt_room_inst_buffer:get_frame(Pid, 3).
