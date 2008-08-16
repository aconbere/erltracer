-module(canvas).

-include_lib("eunit/include/eunit.hrl").

-export([get_corner/4,
         make_screen/4,
         make_camera/2,
         make_camera/3,
         make_canvas/2]).

get_corner([RVertical, RHorizontal], Center, VVector, HVector) ->
    VOffset = vector:mult(VVector, math_ext:floor(RVertical / 2)),
    HOffset = vector:mult(HVector, math_ext:floor(RHorizontal / 2)),
    vector:sum([Center, VOffset, HOffset]).

make_screen([RVertical, RHorizontal], Center, VVector, HVector) ->
    Corner = get_corner([RVertical, RHorizontal], Center, VVector, HVector),
    [[{vector:sub([Corner, vector:mult(VVector, V), vector:mult(HVector, H)])} ||
        H <- lists:seq(1, RHorizontal)] ||
        V <- lists:seq(1, RVertical)].

make_canvas({camera, Origin, Direction, Up}, Resolution) ->
    % we move the Up vector to the Origin on our camera
    DisplacedUp = vector:sum(Up, Origin),
    HVector = vector:cross(DisplacedUp, Direction),
    VVector = vector:cross(Direction, HVector),
    make_screen(Resolution, vector:sum(Origin, Direction), VVector, HVector).

make_camera(Origin, Direction) ->
    make_camera(Origin, Direction, [0,0,1]).
make_camera(Origin, Direction, Up) ->
    {camera, Origin, Direction, Up}.

%% TEST %%

make_camera_test() ->
    make_camera([5,5,5], [-5, -5, -5]),
    make_camera([5,5,5], [-5, -5, -5], [0, 0, 1]).

get_corner_test_() ->
    [?_assert(get_corner([100, 100], [0, 0, 0], [-1, 0, 1], [1, 1, 1]) == [0, 50, 100])
    ].

make_screen_test() ->
    [?_assert(length(make_screen([100, 100], [0, 0, 0], [-1, 0, 1], [1, 1, 1])) == 100)].

make_canvas_test() ->
    make_canvas({camera, [5,5,5], [-5,-5,-5], [0,0,1]}, [100, 100]).
