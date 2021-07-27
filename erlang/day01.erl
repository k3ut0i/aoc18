-module(day01).
-export([part1/1, 
	 part2/1]).
-import(util, [read_lines/1]).

part1(File) ->
    Nums = lists:map(fun erlang:list_to_integer/1,
		     read_lines(File)),
    lists:foldl(fun(E, A) -> E + A end, 0, Nums).

find_rep([], FreqDiff, Freqs) ->
    find_rep(FreqDiff, FreqDiff, Freqs);
find_rep([F|Fs], FreqDiff, [Fc|Fss]) ->
    case lists:member((F+Fc), [Fc|Fss]) of
	true ->
	    F+Fc;
	false ->
	    find_rep(Fs, FreqDiff, [F+Fc, Fc|Fss])
    end.

part2(File) ->
    FreqDiff = lists:map(fun erlang:list_to_integer/1,
			 read_lines(File)),
    find_rep(FreqDiff, FreqDiff, [0]).
