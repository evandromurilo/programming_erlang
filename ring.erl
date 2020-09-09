-module(ring).
-export([benchmark/2]).

-spec benchmark(pos_integer(), pos_integer()) -> 'ok'.

% on my machine, it takes 1 second to pass a message
% around 1000000 nodes
benchmark(N, M) ->
    [H | _] = spawn_ring(self(), N-1, []),
   
    pass_message(H, M, go),
    pass_message(H, 1, die).

pass_message(H, 1, Message) ->
    H ! Message,

    receive Message ->
            io:format("Message passed around!~n")
    end;
pass_message(H, M, Message) ->
    H ! Message,

    receive Message ->
            io:format("Message passed around!~n"),
            pass_message(H, M-1, Message)
    end.

spawn_ring(Previous, 0, Processes) ->
    Processes;
spawn_ring(Previous, Total, Processes) ->
    Next = spawn(fun() -> listen(Previous) end),
    spawn_ring(Next, Total-1, [Next | Processes]).
        
listen(Next) ->
    receive
        die ->
            Next ! die;
        Message ->
            Next ! Message,
            listen(Next)
    end.
