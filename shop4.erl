-module(shop4).
-export([cost/1, total/1]).
-import(lists, [sum/1]).
-include("shop4.hrl").

cost(#shoppingLine{item=Item, quantity=Quantity}) when Quantity > 0 ->
    cost(Item) * Quantity;
cost(#shoppingLine{} = R) ->
    throw({invalidQuantity, R});
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
    sum([ cost(R) || R <- ShoppingList ]).
