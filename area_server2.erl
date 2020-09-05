-module(area_server2).
-export([loop/0, rpc/2]).

rpc(Pid, Message) ->
    Pid ! {self(), Message},
    receive
        {Pid, Response} -> Response
    end.

loop() ->
    receive
        {From, {rectangle, Width, Height}} ->
            From ! {self(), Width * Height},
            loop();
        {From, {square, Size}} ->
            From ! {self(), Size * Size},
            loop();
        {From, Other} ->
            From ! {self(), {error, Other}},
            loop()
    end.
