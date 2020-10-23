-module(module_test).
-export([start/0, call/1]).

start() ->
  io:format("Registering ~p\n", [?MODULE]),
  register(module_test, spawn(
                          fun() -> loop() end
                         )).

call(Message) ->
  ?MODULE ! {self(), Message},

  receive
    {?MODULE, ok} -> ok
  end.

loop() ->
  receive
    {From, Message} ->
      io:put_chars(Message),
      From ! {?MODULE, ok},
      loop()
  end.

