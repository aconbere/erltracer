-module(math_ext).

-include_lib("eunit/include/eunit.hrl").

-export([floor/1,
         ceiling/1]).

floor(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T - 1;
        Pos when Pos > 0 -> T;
        _ -> T
    end.

ceiling(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T;
        Pos when Pos > 0 -> T + 1;
        _ -> T
    end.

floor_test_() ->
    [?_assert(floor(4.2) == 4),
     ?_assert(floor(4.6) == 4)].

ceiling_test_() ->
    [?_assert(ceiling(4.2) == 5),
     ?_assert(ceiling(4.6) == 5)].
