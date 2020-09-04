-module(fac).
-export([main/1]).

-spec fac(non_neg_integer()) -> pos_integer().

main([A]) ->
    I = list_to_integer(atom_to_list(A)),
    F = fac(I),
    io:format("factorial ~w = ~w~n", [I, F]),
    init:stop().

fac(0) ->
    1;
fac(N) ->
    N * fac(N-1).
