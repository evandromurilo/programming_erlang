-module(shop3).
-export([cost/1, total/1]).
-import(lists, [sum/1]).

cost(orange) ->
    1;
cost(apple) ->
    2;
cost(shirt) ->
    3;
cost(socks) ->
    2;
cost(computer) ->
    30;
cost(X) ->
    throw({itemWithoutCost, X}).

total(ShoppingList) ->
    sum([cost(What) * N || {What, N} <- ShoppingList]).
