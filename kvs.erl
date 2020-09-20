-module(kvs).
-export([start/0, store/2, lookup/1]).

-spec start() -> true.
-spec store(_Key, _Value) -> true.
-spec lookup(_Key) -> {ok, _Value} | undefined.

start() ->
    Pid = spawn(fun() -> loop() end),
    register(kvs, Pid).

rpc(Command, Args) ->
    Pid = whereis(kvs),

    Pid ! {self(), Command, Args},

    receive
        {Pid, Message} ->
            Message
    end.

store(Key, Value) ->
    rpc(store, {Key, Value}).

lookup(Key) ->
    rpc(lookup, {Key}).

loop() ->
    receive
        {From, store, {Key, Value}} ->
            put(Key, Value),
            From ! {self(), true},
            loop();
        {From, lookup, {Key}} ->
            From ! {self(), get(Key)},
            loop()
    end.
