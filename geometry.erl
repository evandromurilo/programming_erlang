-module(geometry).
-export([tests/0, area/1, perimeter/1]).

area({square, Side}) ->
    Side * Side;
area({rectangle, Width, Height}) ->
    Width * Height;
area({triangle, Base, Height}) ->
    Base * Height / 2;
area({circle, Radius}) ->
    3.1415 * Radius * Radius.

perimeter({square, Side}) -> Side * 4;
perimeter({rectangle, Width, Height}) -> Width * 2 + Height * 2.

tests() ->
    144 = area({square, 12}),
    12 = area({rectangle, 3, 4}),
    tests_worked.

    
