%%%-------------------------------------------------------------------
%% @copyright (c) 2021 Cezuriku
%%%-------------------------------------------------------------------

-module(rt_room_SUITE).

-include_lib("common_test/include/ct.hrl").

%% Enables ?assert() for readable output
-include_lib("stdlib/include/assert.hrl").

-compile(nowarn_export_all).
-compile(export_all).

%%%-------------------------------------------------------------------
%% Test server callbacks

suite() ->
    [].

init_per_suite(Config) ->
    application:ensure_all_started(rt_room),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(basic, Config) ->
    {ok, RtRoom} = rt_room:create(),
    [{room, RtRoom} | Config];
init_per_group(_GroupName, Config) ->
    Config.

end_per_group(basic, Config) ->
    RtRoom = ?config(room, Config),
    rt_room:stop(RtRoom),
    ok;
end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

groups() ->
    [
        {basic, [], [
            add_remove_player_test,
            move_player_test,
            move_player_late_test
        ]}
    ].

all() ->
    [
        create_stop_room_test,
        {group, basic}
    ].

%%%-------------------------------------------------------------------
%% Test cases

create_stop_room_test(_Config) ->
    {ok, RtRoom} = rt_room:create(),
    rt_room:stop(RtRoom).

add_remove_player_test(Config) ->
    RtRoom = ?config(room, Config),
    {ok, _PlayerId} = rt_room:add_player(RtRoom, ?MODULE, self()),
    ok = rt_room:remove_player(RtRoom, self()).

move_player_test(Config) ->
    RtRoom = ?config(room, Config),
    {ok, PlayerId} = rt_room:add_player(RtRoom, ?MODULE, self()),
    ok = rt_room:move_player(RtRoom, PlayerId, 8, {500, 500}).

move_player_late_test(Config) ->
    RtRoom = ?config(room, Config),
    {ok, PlayerId} = rt_room:add_player(RtRoom, ?MODULE, self()),
    {error, _} = rt_room:move_player(RtRoom, PlayerId, _FramePassed = 0, {500, 500}).
