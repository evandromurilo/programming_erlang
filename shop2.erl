-module(shop2).
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

total(ShoppingList) ->
    sum(map(fun({What, N}) -> cost(What) * N end, ShoppingList)).

sum([H|T]) -> H + sum(T);
sum([]) -> 0.

map(F, []) -> [];
map(F, [H|T]) -> [F(H)|map(F, T)].
