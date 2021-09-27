-module(day05).
-export([main/1,
	removeReact/2]).

polar(C1, C2) ->
    abs(C1 - C2) == 32.

react(Xs) ->
    case react(Xs, {[], 0}) of
	{R, 0} ->
	    R;
	{R, N} when N > 0 ->
	    react(R)
    end.
react([], {A, N}) ->
    {lists:reverse(A), N};
react([X], {A, N}) ->
    {lists:reverse([X|A]), N};
react([X, Y | R], {A, N}) ->
    case polar(X, Y) of
	true ->
	    react(R, {A, N+1});
	false ->
	    react([Y|R], {[X|A], N})
    end.
    
removeReact(C, S) ->
    react(lists:filter(fun (E) -> (E /= C) and (E /= C + 32) end, S)).
    
removeAnalysis(S) ->
    A = lists:map(fun (C) -> {C, length(removeReact(C, S))} end,
		  lists:seq(65, 90)),
    lists:foldl(fun ({C, L}, {CM, LM}) ->
			if L < LM ->
				{C, L};
			   L >= LM ->
				{CM, LM}
			end
		end,
	       {undefined, react(S)}, % could have used first element of A to avoid recomputation
	       A).

main(File) ->
    {ok, Fh} = file:open(File, [read]),
    A = react(string:chomp(io:get_line(Fh, ""))),
    file:close(Fh),
    {length(A), removeAnalysis(A)}.
