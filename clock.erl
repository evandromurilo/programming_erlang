-module(clock).
-export([start/1, start/2, stop/0, tick/0]).

-spec start(pos_integer()) -> 'true'.
-spec start(pos_integer(), fun()) -> 'true'.

start(Time) ->
    register(clock, spawn(fun() -> timer(Time, fun clock:tick/0) end)).

start(Time, Fun) ->
    register(clock, spawn(fun() -> timer(Time, Fun) end)).

tick() ->
    io:format("Tick!~n").

timer(Time, Fun) ->
    receive
        stop ->
            void
    after Time ->
            Fun(),
            timer(Time, Fun)
    end.

stop() ->
    clock ! stop.
          
                                  
