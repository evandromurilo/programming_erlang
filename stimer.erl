-module(stimer).
-export([start/2, cancel/1]).

-spec start(pos_integer(), fun()) -> pid().

start(Time, Fun) ->
    spawn(fun() -> timer(Time, Fun) end).

timer(Time, Fun) ->
    receive
        cancel ->
            void
    after Time ->
        Fun()
    end.

cancel(Timer) ->
    Timer ! cancel.
