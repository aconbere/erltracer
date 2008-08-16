-module(dist).
-export([pmap/2]).

pmap(F, L) ->
    S = self(),
    Pids = lists:map(fun(I) ->
                         spawn(fun() -> do_f(S, F, I) end)
                     end, L),
    gather(Pids).

gather([H|T]) ->
    receive
        {H, Ret} -> [Ret|gather(T)]
    end;
gather([]) ->
    [].

do_f(Parent, F, I) ->
    Parent ! {self(), (catch F(I))}.
