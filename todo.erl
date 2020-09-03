-module(todo).
-export([advance/1, count_characters/1]).
-include("todo.hrl").

-spec advance(tuple()) -> tuple().

advance(#todo{status=backlog} = R) ->
    R#todo{status=prework};
advance(#todo{status=prework} = R) ->
    R#todo{status=in_progress};
advance(#todo{status=in_progress} = R) ->
    R#todo{status=review};
advance(#todo{status=review} = R) ->
    R#todo{status=done}.

count_characters(#todo{text=Text}) ->
    count_characters(Text, #{}).

count_characters([H|T], X) ->
    count_characters(T, maps:update_with(H, fun(N) -> N+1 end, 1, X));
count_characters([], X) ->
    X.
