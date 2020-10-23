-module(lib_misc).
-export([for/3, qsort/1, perms/1, max/2, filter/2, a_filter/2, odds_and_evens/1, tuple_to_list/1, a_tuple_to_list/1, time_func/1, datetime_string/0, map_search_pred/2, size_of/1, join/2, count/1, unique/1, map_joining/2, on_exit/2, start/1, keep_alive/2, necrology_spawn/3, a_necrology_spawn/3, butterfly_spawn/4, immortal_spawn/0, repeat/2, replace_first/3, remove_first/2]).
-export([consult/1, aconsult/1]).
-import(erlang, [system_time/1]).

-spec for(Begin, Max, fun((integer()) -> Y)) -> [Y] when
      Begin :: integer(),
      Max :: integer().

-spec qsort([X]) -> [X].

-spec max(integer(), integer()) -> integer().

-spec filter(fun((X) -> boolean()), [X]) -> [X].

-spec odds_and_evens([integer()]) -> {Odds, Evens} when
      Odds :: [integer()],
      Evens :: [integer()].

-spec tuple_to_list(tuple()) -> list().

-spec time_func(fun()) -> MicrosecondsElapsed when
      MicrosecondsElapsed :: integer().

-spec datetime_string() -> string().

-spec map_search_pred(#{any() => V}, fun((V) -> boolean())) -> not_found | V.

-spec size_of(list()) -> integer().

-spec join(list(), list()) -> list().

-spec count(list()) -> map().

-spec unique(list()) -> list().

-spec map_joining(list(), fun((any()) -> Y)) -> [Y].

-spec repeat(non_neg_integer(), fun(() -> Y)) -> [Y].

-spec replace_first(any(), any(), list()) -> list().

-spec remove_first(any(), list()) -> list().

for(Max, Max, F) -> [F(Max)];
for(I, Max, F) -> [F(I)|for(I+1, Max, F)].

qsort([]) -> [];
qsort([Pivot|T]) ->
    qsort([X || X <- T, X < Pivot])
    ++ [Pivot] ++
    qsort([X || X <- T, X > Pivot]).
                    

perms([]) -> [[]];
perms(L) -> [[H|T] || H <- L,
                      T <- perms(L--[H])].

max(X, Y) when X > Y -> X;
max(_X, Y) -> Y.

filter(_P, []) -> [];
filter(P, [H|T]) -> filter1(P(H), H) ++ filter(P, T).

filter1(true, H) -> [H];
filter1(false, _H) -> [].

a_filter(P, [H|T]) ->
    case P(H) of
        true -> [H|a_filter(P, T)];
        false -> a_filter(P, T)
    end;
a_filter(_P, []) ->
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

% it's a proposed exercise, but the json BIFS were not
% actually implemented in the erlang language
%
% read_json_config(Filename) ->
%     Content = file:read_file(Filename),
%     maps:from_json(Content).
% 
% test_read_json_config() ->
%     M = read_json_config("./test.json"),
%     #{name := "MainServer"} = M.

map_search_pred(Map, Pred) ->
    first(maps:to_list(Map), Pred).

first([H|T], Pred) ->
    case Pred(H) of
        true -> H;
        false -> first(T, Pred)
    end;
first([], _Pred) -> not_found.

size_of(L) ->
    size_of(L, 0).

size_of([], N) ->
    N;
size_of([_H|T], N) ->
    size_of(T, N+1).

join([], L2) ->
    L2;
join([H|T], L2) ->
    join(T, [H|L2]).

count(L) ->
    count(L, #{}).

count([], X) ->
    X;
count([H|T], X) ->
    count(T, maps:update_with(H, fun(N) -> N+1 end, 1, X)).

unique(L) ->
    [K || {K, V} <- maps:to_list(count(L)),
                    V =:= 1].

map_joining([H|T], F) ->
    join(F(H), map_joining(T, F));
map_joining([], _F) ->
    [].

on_exit(Pid, F) ->
    spawn(fun() ->
                 Ref = monitor(process, Pid),
                 receive {'DOWN', Ref, process, Pid, Why} ->
                         F(Why)
                 end
         end).

start(Fs) ->
    spawn(fun() ->
                  [spawn_link(F) || F <- Fs],
                  receive
                      after
                          infinity ->
                               true
                      end
          end).

keep_alive(Name, Fun) ->
    register(Name, Pid = spawn(Fun)),
    on_exit(Pid, fun(_Why) ->
                         keep_alive(Name, Fun) end).

necrology_spawn(Mod, Func, Args) ->
    Pid = spawn(Mod, Func, Args),
    
    spawn(fun() ->
                  StartTime = system_time(microsecond),
                  Ref = monitor(process, Pid),
                  receive {'DOWN', Ref, process, Pid, Why} ->
                          Elapsed = system_time(microsecond) - StartTime,

                          io:format("The process ~p died after ~p microseconds with ~p~n", [Pid, Elapsed, Why])
                  end
    end),

    Pid.

a_necrology_spawn(Mod, Func, Args) ->
    Pid = spawn(Mod, Func, Args),
    StartTime = system_time(microsecond),

    on_exit(Pid, fun(Why) ->
                         Elapsed = system_time(microsecond) - StartTime,
                         io:format("The process ~p died after ~p microseconds with ~p~n", [Pid, Elapsed, Why])
                 end),

    Pid.

butterfly_spawn(Mod, Func, Args, Seconds) ->
    Pid = spawn(Mod, Func, Args),
    Milliseconds = Seconds * 1000,
    
    spawn(fun() ->
                  link(Pid),

                  receive
                  after Milliseconds ->
                          true
                  end,
                  io:format("Killing ~p, ~p seconds already elapsed~n", [Pid, Seconds]),
                  exit(Pid, timeout)
          end),

    Pid.

immortal_loop() ->
    receive
    after 5000 ->
            io:format("I'm still running!~n")
    end,
    immortal_loop().

immortal_spawn() ->
    Pid = spawn(fun() -> immortal_loop() end),
    register(immortal, Pid),

    spawn(fun() ->
                  Ref = monitor(process, Pid),
                  receive {'DOWN', Ref, process, Pid, _Why} ->
                          io:format("Fool! You cannot kill the immortal!!~n"),
                          immortal_spawn()
                  end
          end),
    immortal.

repeat(0, _Func) ->
    [];
repeat(Times, Func) ->
    [Func() | repeat(Times-1, Func)].

replace_first(_Old, _New, []) ->
    [];
replace_first(Old, New, [Old|T]) ->
    [New|T];
replace_first(Old, New, [H|T]) ->
    [H|replace_first(Old, New, T)].

remove_first(_Needle, []) ->
    [];
remove_first(Needle, [Needle|T]) ->
    T;
remove_first(Needle, [H|T]) ->
    [H|remove_first(Needle, T)].

consult(File) ->
    {ok, Device} = file:open(File, read),
    consult(Device, []).

consult(Device, Terms) ->
    case io:read(Device, '') of
        eof -> 
            file:close(Device),
            Terms;
        {ok, Term} ->
            consult(Device, [Term | Terms])
    end.

aconsult(File) ->
    {ok, Device} = file:open(File, read),
    aconsult1(Device).

aconsult1(Device) ->
    case io:read(Device, '') of
        {ok, Term} ->
            [Term | aconsult1(Device)];
        eof ->
            file:close(Device),
            []
    end.
