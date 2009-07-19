-module(sphere).

-include_lib("eunit/include/eunit.hrl").

-export([intersect/2,
         make_sphere/2,
         make_sphere/3]).

make_sphere(Origin, Radius) ->
    {sphere, [Origin, Radius, [255, 0, 0]]}.

make_sphere(Origin, Radius, Color) ->
    {sphere, [Origin, Radius, Color]}.


intersect(Ray, Sphere) ->
    case find_intersection(Ray, Sphere) of
        {true, Time} ->
            {true, {Ray, Sphere, Time, {point, vector:mult(Ray, Time)}}};
        false -> false
    end.

find_intersection({ray, ROrigin, RDirection}, {sphere, [SOrigin, SRadius, _Color]}) ->
    % Shift the world such that the ray's origin is at the
    % origin of the coordinate system
    SOrigin1 = vector:sub(ROrigin, SOrigin), 

    RDirectionUnit = vector:unit(RDirection),

    A = vector:dot(RDirectionUnit, RDirectionUnit),
    B = 2 * vector:dot(RDirectionUnit, SOrigin1),
    C = vector:dot(SOrigin1, SOrigin1) - math:pow(SRadius, 2),

    case math_ext:quadratic(A, B, C) of
        {ok, [T1, T2]} -> T = case T1 < T2 of
                                  true -> T1;
                                  false -> T2
                              end,
                          {true, T};
        {error, _Reason} -> false
    end.

intersect_test_() ->
    [?_assert(intersect({ray, [0,0,0], [1,1,1]}, make_sphere([10, 10, 10], 1)) == ok)].
