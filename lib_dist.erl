-module(lib_dist).
-export([ping_all/1, ping_all_connected/0]).

-type ping_response() :: pong | pang.

-spec ping_all([node()]) -> [ping_response()].
-spec ping_all_connected() -> [ping_response()].

ping_all([]) ->
	[];
ping_all([H|T]) ->
	[net_adm:ping(H) | ping_all(T)].

ping_all_connected() ->
	ping_all(nodes()).
