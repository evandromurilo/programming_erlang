-module(math_functions).
-export([even/1, odd/1, even_or_odd/1, filter/2, split/1, a_split/1, test/0]).

even(N) -> N rem 2 =:= 0.
odd(N) -> N rem 2 =:= 1.

even_or_odd(N) when N rem 2 =:= 0 -> even;
even_or_odd(N) -> odd.

filter(F, L) ->
    [X || X <- L,
          F(X)].

split(L) ->
    {filter(fun(N) -> even(N) end, L), filter(fun(N) -> odd(N) end, L)}.

a_split(L) -> a_split_acc(L, [], []).

a_split_acc([H|T], Evens, Odds) when H rem 2 =:= 0 ->
    a_split_acc(T, [H|Evens], Odds);
a_split_acc([H|T], Evens, Odds) ->
    a_split_acc(T, Evens, [H|Odds]);
a_split_acc([], Evens, Odds) -> {Evens, Odds}.

test() ->
    true = even(2),
    false = even(3),
    false = odd(4),
    true = odd(5),
    even = even_or_odd(2),
    odd = even_or_odd(3),
    tests_worked.
   
