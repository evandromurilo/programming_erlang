-module(shop5).
-export([cost/1, total/1]).
-import(lists, [sum/1]).

-spec total([shoppingLine()]) -> integer().

-type item() :: orange | apple | shirt | socks | computer.
-type shoppingLine() :: [{item(), Quantity :: integer()}].

cost({Item, Quantity}) when Quantity > 0 ->
    cost(Item) * Quantity;
cost({_, _} = R) ->
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
