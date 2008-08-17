-module(sphere).

-include_lib("eunit/include/eunit.hrl").

-export([intersect/2]).

make_sphere(Origin, Radius) ->
    {sphere, [Origin, Radius]}.

intersect({ray, Origin, Direction}, {sphere, [Origin, Radius]}) ->
    true.

intersect_test() ->
    ?_assert(intersect({ray, [0,0,0], [1,1,1]}, make_sphere([10, 10, 10], 1)) == false).
