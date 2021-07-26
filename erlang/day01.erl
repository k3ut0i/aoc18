-module(day01).
-export([part1/1, 
	 part2/1,
	read_lines/1]).

read_lines(File) ->
    {ok, Fh} = file:open(File, [read]),
    Ls = read_lines(Fh, []),
    file:close(Fh),
    Ls.
read_lines(Fh, Ls) ->
    case io:get_line(Fh, "") of
	eof ->
	    lists:reverse(Ls);
	{error, Des} ->
	    error(Des);
	Line ->
	    read_lines(Fh, [string:chomp(Line)|Ls])
    end.
    
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
