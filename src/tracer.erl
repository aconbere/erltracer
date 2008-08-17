-module(tracer).

-include_lib("eunit/include/eunit.hrl").

-export([trace/3,
         make_ray/2,
         shoot/2,
         trace_row/3
]).

trace(Camera, Resolution, Scene) ->
    Canvas = canvas:make_canvas(Camera,
                                Resolution),

    dist:pmap(fun(Row) -> trace_row(Row, Camera, Scene) end, Canvas).

trace_row(Row, {camera, COrigin, _Direction, _Up}, Scene) ->
    dist:pmap(fun(Pixel) -> shoot(make_ray(COrigin, Pixel), Scene) end, Row).

make_ray(Origin, Direction) ->
    {ray, Origin, Direction}.

shoot(Ray, Scene) ->
    pmap(fun(Object) -> intersect(Ray, Object) end, Scene).

intersect(Ray, {type, Args} = Object) ->
    apply(type, intersect, Ray, Object).

ray_test() ->
    ?_assert(make_ray([0,0,0], [1,1,1]) == {ray, [0,0,0], [1,1,1]}).

shoot_test() ->
    ?_assert(shoot(make_ray([-10, -10, -10], [1,1,1]), [{sphere, [[0,0,0], 1]}]) == true). 

trace_row_test() ->
    trace_row([[1,1,1], [1,1,2], [1,1,3]], canvas:make_camera([0,0,0], [1,1,1]), [{sphere, [[0,0,0], 1]}]).

trace_test() ->
    trace(canvas:make_camera([-500, -500, -500], [1,1,1], [0,0,1]), [100, 100], []).
