-module(rt_room_observer).

-callback handle_new_frame(
    Pid :: pid(),
    Frame :: integer(),
    AddedPlayers :: rt_room:players(),
    UpdatedPlayers :: rt_room:players(),
    RemovedPlayers :: [rt_room:player_id()]
) -> ok.
