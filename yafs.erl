-module(yafs).
-export([start/0, ls/1, read_file/2, write_file/3, loop/0]).

start() ->
    spawn(yafs, loop, []).

ls(Pid) ->
    rpc(Pid, {ls}).

read_file(Pid, Filename) ->
    rpc(Pid, {read_file, Filename}).

write_file(Pid, Filename, Content) ->
    rpc(Pid, {write_file, Filename, Content}).

rpc(Pid, Args) ->
    Pid ! {self(), Args},
    receive {Pid, Response} -> Response end.

loop() ->
    receive
        {From, {ls}} ->
            From ! {self(), file:list_dir("files")},
            loop();
        {From, {read_file, Filename}} ->
            From ! {self(), file:read_file("files/" ++ Filename)},
            loop();
        {From, {write_file, Filename, Content}} ->
            From ! {self(), file:write_file("files/" ++ Filename, Content)},
            loop()
    end.
