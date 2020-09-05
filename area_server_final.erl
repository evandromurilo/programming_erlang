-module(area_server_final).
-export([start/0, area/2, loop/0]).

-type square_request() :: {square, Side::number()}.
-type rectangle_request() :: {rectangle, Width::number(), Height::number()}.

-spec area(Server, What) -> Response | Error when
      Server :: pid(),
      What :: square_request() | rectangle_request(),
      Response :: number(),
      Error :: {error, _Cause}.

-spec start() -> pid().

area(Pid, What) ->
    rpc(Pid, What).

start() ->
    spawn(area_server_final, loop, []).

rpc(Pid, What) ->
    Pid ! {self(), What},
    receive
        {Pid, Response} -> Response
    end.

loop() ->
    receive
        {From, {rectangle, Width, Height}} ->
            From ! {self(), Width * Height},
            loop();
        {From, {square, Side}} ->
            From ! {self(), Side * Side},
            loop();
        {From, Other} ->
            From ! {self, {error, Other}},
            loop()
    end.
