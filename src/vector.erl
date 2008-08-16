-module(vector).

-include_lib("eunit/include/eunit.hrl").

-export([mult/2,
         sum/2,
         sum/1,
         sub/2,
         sub/1,
         dot/2,
         cross/2]).

mult(X, Y) when is_list(X) and is_number(Y) ->
    [X1 * Y || X1 <- X];
mult(X, Y) when is_number(X) and is_list(Y) ->
    mult(Y, X).

sum(X, Y) when is_list(X) and is_list(Y) ->
    [X1 + Y1 || {X1, Y1} <- lists:zip(X, Y)].

sum(X) when is_list(X) ->
    lists:foldl(fun(Elem, AccIn) -> sum(Elem, AccIn) end, [0,0,0], X).

sub(X, Y) when is_list(X) and is_list(Y) ->
    [X1 - Y1 || {X1, Y1} <- lists:zip(X, Y)].

sub(X) when is_list(X) ->
    lists:foldl(fun(Elem, AccIn) -> sub(Elem, AccIn) end, [0,0,0], X).

dot(X, Y) when is_list(X) and is_list(Y) ->
    lists:sum([X1 * Y1 || {X1, Y1} <- lists:zip(X, Y)]).

cross([X1, X2, X3], [Y1, Y2, Y3]) ->
    [(X2 * Y3) - (X3 * Y2), (X3 * Y1) - (X1 * Y3), (X1 * Y2) - (X2 * Y1)].

%% TESTS %%
cross_test_() ->
    [?_assert(cross([1, 2, 3], [5, 4, 2]) == [-8, 13, -6]),
     ?_assert(cross([1, 2, 3], [2, 4, 6]) == [0, 0, 0])
    ].

dot_test_() ->
    [?_assert(dot([1, 0, -1], [1, 1, 1]) == 0),
     ?_assert(dot([1, 1, 1], [1, 1, 1]) == 3)
    ].

sum_test_() ->
    [?_assert(sum([1,1,1], [1,1,1]) == [2,2,2]),
     ?_assert(sum([[1,1,1], [1,1,1]]) == [2,2,2])
    ].

sub_test_() ->
    [?_assert(sub([1,1,1], [1,1,1]) == [0,0,0]),
     ?_assert(sub([[1,1,1], [1,1,1]]) == [0,0,0])
    ].

mult_test_() ->
    [?_assert(mult([1,1,1], 4) == [4,4,4]),
     ?_assert(mult(4, [1,1,1]) == [4,4,4])
    ].
