-module(array2d).
-export([new/2,
	 set/4,
	 get/3,
	 map_c/2]).

new(X, Y) ->
    {array:new([{size, X*Y}, {default, 0}]), X, Y}.

set({A, Xm, Ym}, V, X, Y) when X < Xm, Y < Ym ->
    {array:set(X*Ym+Y, V, A), Xm, Ym}.

get({A, Xm, Ym}, X, Y) when X < Xm, Y < Ym ->
    array:get(X*Ym+Y, A).

map_c(F, {A, Xm, Ym}) ->
    {array:map(fun (_, X) -> F(X) end, A), Xm, Ym}.
