-module(asupervisor).
-export([start/4, list_workers/1, die/1]).
-import(lib_misc, [repeat/2, replace_first/3, remove_first/2]).
-import(lists, [foreach/2]).

start(Mod, Func, Args, Total) ->
    spawn(fun() -> spawn_workers(Mod, Func, Args, Total) end).

rpc(Pid, Action) ->
    Pid ! {self(), Action},
    receive {Pid, Response} -> Response end.

list_workers(Pid) ->
    rpc(Pid, list_workers).

die(Pid) ->
    rpc(Pid, die).

spawn_workers(Mod, Func, Args, Total) ->
    Pids = repeat(Total, fun() ->
                                 {Pid, _Ref} = spawn_monitor(Mod, Func, Args),
                                 Pid
                         end),
    loop(Mod, Func, Args, Pids).

loop(_Mod, _Func, _Args, []) ->
    io:format("No workers to mantain, exiting...~n");
loop(Mod, Func, Args, Pids) ->
    receive
        {'DOWN', _Ref, process, DeadPid, supervisor_exit} ->
            io:format("Killed worker ~p~n", [DeadPid]),
            loop(Mod, Func, Args, remove_first(DeadPid, Pids));
        {'DOWN', _Ref, process, DeadPid, _Why} ->
            io:format("Restarting dead worker ~p~n", [DeadPid]),
            NewPid = spawn(Mod, Func, Args),
            monitor(process, NewPid),
            loop(Mod, Func, Args, replace_first(DeadPid, NewPid, Pids));
        {From, list_workers} ->
            From ! {self(), Pids},
            loop(Mod, Func, Args, Pids);
        {From, die} ->
            foreach(fun(Pid) -> exit(Pid, supervisor_exit) end, Pids),
            From ! {self(), ok},
            loop(Mod, Func, Args, Pids)
    end.
