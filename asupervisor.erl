-module(asupervisor).
-export([start/5, list_workers/1, die/1]).
-import(lib_misc, [repeat/2, replace_first/3, remove_first/2]).
-import(lists, [foreach/2]).


-spec start(_Mod, _Func, _Args, Total, Strategy) -> pid() when
      Total :: pos_integer(),
      Strategy :: strategy().

-type strategy() :: replace_all | replace_single.

-spec list_workers(pid()) -> [pid()].

-spec die(pid()) -> ok.


start(Mod, Func, Args, Total, Strategy) ->
    spawn(fun() -> spawn_workers(Mod, Func, Args, Total, Strategy) end).

rpc(Pid, Action) ->
    Pid ! {self(), Action},
    receive {Pid, Response} -> Response end.

list_workers(Pid) ->
    rpc(Pid, list_workers).

die(Pid) ->
    rpc(Pid, die).

spawn_workers(Mod, Func, Args, Total, Strategy) ->
    Pids = repeat(Total, fun() ->
                                 {Pid, _Ref} = spawn_monitor(Mod, Func, Args),
                                 Pid
                         end),

    loop(Mod, Func, Args, Pids, Strategy).

loop(_Mod, _Func, _Args, [], Strategy) ->
    io:format("No workers to mantain, exiting...~n");
loop(Mod, Func, Args, Pids, Strategy) ->
    receive
        {'DOWN', _Ref, process, DeadPid, supervisor_exit} ->
            io:format("Killed worker ~p~n", [DeadPid]),
            loop(Mod, Func, Args, remove_first(DeadPid, Pids), Strategy);
        {'DOWN', _Ref, process, DeadPid, _Why} ->
            case Strategy of
                replace_all ->
                    io:format("Restarting all workers (~p died)~n", [DeadPid]),
                    foreach(fun(Pid) -> exit(Pid, supervisor_exit) end, Pids),
                    spawn_workers(Mod, Func, Args, length(Pids), replace_all);
                replace_single ->
                    io:format("Restarting dead worker ~p~n", [DeadPid]),
                    NewPid = spawn(Mod, Func, Args),
                    monitor(process, NewPid),
                    loop(Mod, Func, Args, replace_first(DeadPid, NewPid, Pids), replace_single)
            end;
        {From, list_workers} ->
            From ! {self(), Pids},
            loop(Mod, Func, Args, Pids, Strategy);
        {From, die} ->
            foreach(fun(Pid) -> exit(Pid, supervisor_exit) end, Pids),
            From ! {self(), ok},
            loop(Mod, Func, Args, Pids, Strategy)
    end.
