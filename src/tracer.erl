-module(tracer).

-include_lib("eunit/include/eunit.hrl").

-export([trace/4,
         make_ray/2,
         shoot/2,
         intersect/2,
         trace_row/3,
         output_trace/0
]).

trace(Camera, Resolution, Scene, Lights) ->
    Canvas = canvas:make_canvas(Camera,
                                Resolution),

    dist:pmap(fun(Row) -> trace_row(Row, Camera, Scene, Lights) end, Canvas).

trace_row(Row, {camera, COrigin, _Direction, _Up}, Scene, Lights) ->
    % this map goes and finds ray intersections with objects
    Shoot = fun(Pixel) -> shoot(make_ray(COrigin, Pixel), Scene) end,
    Intersections = dist:pmap(Shoot, Row),

    % now we need to go find if any of those intersections are lit
    % first we'll take the data structure returned from Intersections
    ShootLights = fun(Intersection) -> shoot(). 
                                
make_ray(Origin, Direction) ->
    {ray, Origin, Direction}.

shoot(Ray, Objects) ->
    Intersections = dist:pmap(fun(Object) -> intersect(Ray, Object) end, Objects),
    reduce_intersections(Intersections).

reduce_intersections(IntList) ->
    MinIntersection = fun({true, Int}, {true, Int2}) ->
        {_Ray, _Object, Time } = Int,
        {_Ray, _Object, Time2} = Int2,
        Time < Time2
    end,

    NewList = lists:filter(fun(Elem) -> Elem =/= false end, IntList),

    case length(NewList) > 1 of
        true ->
            lists:first(lists:sort(MinIntersection, NewList));
        false ->
            NewList
    end.
        
intersect(Ray, {Type, Args} = Object) ->
    apply(Type, intersect, [Ray, Object]).



%% TESTS %%

intersect_test() ->
    ?_assert(intersect(make_ray([0,0,0], [1,1,1]), sphere:make_sphere([6,6,6], 2)) == true).

ray_test() ->
    ?_assert(make_ray([0,0,0], [1,1,1]) == {ray, [0,0,0], [1,1,1]}).

shoot_test() ->
    ?_assert(shoot(make_ray([-10, -10, -10], [1,1,1]), [sphere:make_sphere([10,10,10], 1)]) == [true]). 

trace_row_test() ->
    trace_row([[1,1,1], [1,1,2], [1,1,3]],
              canvas:make_camera([0,0,0], [1,1,1]),
              [sphere:make_sphere([10,10,10],1)]).

trace_test() ->
    trace(canvas:make_camera([-500, -500, -500], [1,1,1], [0,0,1]),
          [100, 100],
          [sphere:make_sphere([100,100,100], 100),
            sphere:make_sphere([1,1,500], 200)]).

%% OUTPUT %%
output_trace() ->
    io:format("~p", [trace_test()]).
