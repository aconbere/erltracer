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

discriminant(A, B, C) ->
    math:pow(B, 2) - (4 * A * C).

quadratic(A, B, C) ->
    case discriminant(A, B, C) of
        Disc when Desc >= 0 ->
            SqrtDisc = math:sqrt(Disc),
            T1 = (-B + SqrtDisc) / (2 * A),
            T2 = (-B - SqrtDisc) / (2 * A),
            {ok, [T1, T2]};
        _ ->
            {error, "Discrimant less than zero"}
    end.
            
discriminant_test_() ->
    [?_assert(discriminant(3, 4, 5) == 14)].

floor_test_() ->
    [?_assert(floor(4.2) == 4),
     ?_assert(floor(4.6) == 4)].

ceiling_test_() ->
    [?_assert(ceiling(4.2) == 5),
     ?_assert(ceiling(4.6) == 5)].
