-module(shop).
-export([cost/1, total/1]).

cost(orange) ->
    1;
cost(apple) ->
    2;
cost(shirt) ->
    3;
cost(socks) ->
    2;
cost(computer) ->
    30.

total([{What, N}|T]) ->
    cost(What) * N + total(T);
total([]) ->
    0.
