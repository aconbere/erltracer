-module(color).

-export([new/1]).

-define(RED, [255,0,0]).
-define(GREEN, [0,255,0]).
-define(BLUE, [0,0,255]).
-define(BLACK, [0,0,0]).
-define(WHITE, [255,255,255]).

new([Red, Green, Blue]) ->
    {color, [Red, Green, Blue]};

new(Color) when is_atom(Color) ->
    case Color of
        red -> {color, ?RED};
        green -> {color, ?GREEN};
        blue -> {color, ?BLUE};
        black -> {color, ?BLACK};
        white -> {color, ?WHITE}
    end.
