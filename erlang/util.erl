-module(util).
-export([read_lines/1,
	search_pair/2,
	 freq_count/1]).

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

search_pair(_, []) ->
    [];
search_pair(F, [X|Xs]) ->
    LX = lists:filter(fun (A) -> F(X, A) end, Xs),
    lists:map(fun (A) -> {X, A} end, LX) ++ search_pair(F, Xs).

freq_count([]) ->
    dict:new();
freq_count([X|Xs]) ->
    dict:update(X, fun (V) -> V + 1 end, 1, freq_count(Xs)).

