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

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

groups() ->
    [{basic, [], [add_remove_player_test]}].

all() ->
    [
        create_room_test,
        {group, basic}
    ].

%%%-------------------------------------------------------------------
%% Test cases

create_room_test(_Config) ->
    {ok, _RtRoom} = rt_room:create().

add_remove_player_test(Config) ->
    RtRoom = ?config(room, Config),
    {ok, _PlayerId} = rt_room:add_player(RtRoom, self()),
    ok = rt_room:remove_player(RtRoom, self()).
