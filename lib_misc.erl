-module(lib_misc).
-export([for/3, qsort/1, perms/1, max/2, filter/2, a_filter/2, odds_and_evens/1, tuple_to_list/1, a_tuple_to_list/1, time_func/1, datetime_string/0]).
-import(erlang, [system_time/1]).

for(Max, Max, F) -> [F(Max)];
for(I, Max, F) -> [F(I)|for(I+1, Max, F)].

qsort([]) -> [];
qsort([Pivot|T]) ->
    qsort([X || X <- T, X < Pivot])
    ++ [Pivot] ++
    qsort([X || X <- T, X > Pivot]).
                    

perms([]) -> [];
perms(L) -> [[H|T] || H <- L,
                      T <- perms(L--[H])].

max(X, Y) when X > Y -> X;
max(X, Y) -> Y.

filter(P, []) -> [];
filter(P, [H|T]) -> filter1(P(H), H) ++ filter(P, T).

filter1(true, H) -> [H];
filter1(false, H) -> [].

a_filter(P, [H|T]) ->
    case P(H) of
        true -> [H|a_filter(P, T)];
        false -> a_filter(P, T)
    end;
a_filter(P, []) ->
    [].

odds_and_evens(L) ->
    odds_and_evens_acc(L, [], []).

odds_and_evens_acc([H|T], Odds, Evens) ->
    case (H rem 2) of
        1 -> odds_and_evens_acc(T, [H|Odds], Evens);
        0 -> odds_and_evens_acc(T, Odds, [H|Evens])
    end;
odds_and_evens_acc([], Odds, Evens) ->
    {Odds, Evens}.

tuple_to_list(T) ->
    tuple_to_list1(T, 1).

tuple_to_list1(T, N) when tuple_size(T) < N -> [];
tuple_to_list1(T, N) ->
    [element(N, T)|tuple_to_list1(T, N+1)].
    
a_tuple_to_list(T) ->
    [element(N, T) || N <- lists:seq(1, tuple_size(T))].

time_func(F) ->
    StartTime = system_time(microsecond),
    F(),
    system_time(microsecond) - StartTime.

datetime_string() ->
    date_string(erlang:date()) ++ " " ++ time_string(erlang:time()).

date_string({Year, Month, Day}) ->
    str(Day) ++ "/" ++ str(Month) ++ "/" ++ str(Year).

time_string({Hour, Minute, Second}) ->
    str(Hour) ++ ":" ++ str(Minute) ++ ":" ++ str(Second).
   
str(N) when N >= 10 -> integer_to_list(N);
str(N) when N < 10 -> "0" ++ integer_to_list(N).