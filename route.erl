-module(route).
-export([plan_route/2]).

-spec plan_route(From:: point(), To:: point()) -> route().

-type direction() :: north | south | west | east.
-type point() :: {integer(), integer()}.
-type route() :: [{go, direction(), integer()}].

plan_route(From, To) ->
    plan_route(From, To, []).

plan_route(To, To, Plan) ->
    Plan;
plan_route({X1, Y1}, {X2, Y2}=To, Plan) when Y1 > Y2 ->
    plan_route({X1, Y2}, To, [{go, south, Y1-Y2} | Plan]);
plan_route({X1, Y1}, {X2, Y2}=To, Plan) when Y1 < Y2 ->
    plan_route({X1, Y2}, To, [{go, north, Y2-Y1} | Plan]);
plan_route({X1, Y1}, {X2, Y2}=To, Plan) when X1 > X2 ->
    plan_route({X2, Y1}, To, [{go, east, X1-X2} | Plan]);
plan_route({X1, Y1}, {X2, Y2}=To, Plan) when X1 < X2 ->
    plan_route({X2, Y1}, To, [{go, west, X2-X1} | Plan]).

