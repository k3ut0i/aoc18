-module(day03).
-export([main/1,
	 get_claims/1,
	 process_claims/1]).
-import(util, [read_lines/1]).

parse_claim(Str) ->
    {match, L} = re:run(Str,
			"#([0-9]+) @ ([0-9]+),([0-9]+): ([0-9]+)x([0-9]+)",
			[{capture, all_but_first, list}]),
    to_rect(lists:map(fun list_to_integer/1, L)).

to_rect([Id, Lo, To, W, H]) ->
    {Id, {Lo, Lo+W-1}, {To, To+H-1}, 0}.

line_intersection_length({La1, La2}, {Lb1, Lb2}) ->
    if (La1 =< Lb1) and (Lb1 =< La2) ->
	    La2 - Lb1;
       (Lb1 =< La1) and (La1 =< Lb2) ->
	    Lb2 - La1;
       true -> false
    end.

lines_intersect({La1, La2}, {Lb1, Lb2}) ->
    (La1 =< Lb1) and (Lb1 =< La2) or  (Lb1 =< La1) and (La1 =< Lb2).

claim_intersect(H1, V1, H2, V2) ->
    lines_intersect(H1, H2) and  lines_intersect(V1, V2).

get_claims(File) ->
    Ls = lists:map(fun (X) -> parse_claim(X) end,
	     util:read_lines(File)),
    T = ets:new(claims, [ordered_set]),
    lists:foldl(fun (X, A) -> 
			ets:insert(A, X), A
		end,
		T,
		Ls).

process_claims(T) ->
    process_claims(T, ets:first(T)).
process_claims(T, K) ->
    Knext = ets:next(T, K),
    case Knext of
	'$end_of_table' ->
	    T;
	_ ->
	    update_claim_with(T, K, Knext),
	    process_claims(T, Knext)
    end.
    

update_claim_with(T, K1, K2) ->
    [[H1, V1, C1]] = ets:match(T, {K1, '$1', '$2', '$3'}),
    [[H2, V2, C2]] = ets:match(T, {K2, '$1', '$2', '$3'}),
    case claim_intersect(H1, V1, H2, V2) of
       true ->
	    ets:insert(T, {K1, H1, V1, 1+C1}),
	    ets:insert(T, {K2, H2, V2, 1+C2});
       false ->
	    true
    end,
    Knext = ets:next(T, K2),
    case Knext of
	'$end_of_table' ->
	    T;
	_ ->
	    update_claim_with(T, K1, Knext)
    end.

in_claim(N, {H1, H2}, {V1, V2}) ->
    X = N div 1000,
    Y = N rem 1000,
    (H1 =< X) and (X =< H2) and (V1 =< Y) and (Y =< V2).
is_twice(T, N) ->
    is_twice(T, N, ets:first(T), zero).
is_twice(_, _, '$end_of_table', _) ->
    false;
is_twice(T, N, K, zero) ->
    [[H, V]] = ets:match(T, {K, '$1', '$2', '_'}),
    case in_claim(N, H, V) of
	true ->
	    is_twice(T, N, ets:next(T, K), one);
	false ->
	    is_twice(T, N, ets:next(T, K), zero)
    end;
is_twice(T, N, K, one) ->
        [[H, V]] = ets:match(T, {K, '$1', '$2', '_'}),
    case in_claim(N, H, V) of
	true ->
	    true;
	false ->
	    is_twice(T, N, ets:next(T, K), one)
    end.

%% XXX: Extremely inefficient - 20 minutes to run on my core2duo
%% but uses almost no extra memory after ets table is initialized
count(T) ->
    count(T, 0, 0).
count(T, 1000000, A) ->
    A;
count(T, N, A) when N < 1000000->
    case (N rem 10000) of
	0 -> io:fwrite("~w ~w~n", [N, A]);
	_ -> true
    end,
    case is_twice(T, N) of
	true ->
	    count(T, N+1, A+1);
	false ->
	    count(T, N+1, A)
    end.

explode_rect({H1, H2}, {V1, V2}) ->
    [{X, Y} || X <- lists:seq(H1, H2),
	       Y <- lists:seq(V1, V2)].
%%% XXX: runs in a couple of seconds.
count2(T) ->
    count2(T, array2d:new(1000, 1000), ets:first(T)).
count2(T, {A, _, _}, '$end_of_table') -> %% using array2d internal rep here
    array:foldl(fun (_, X, C) -> %% to lazy to wrap it in array2d
			if X > 1 -> C+1;
			   X =< 1 -> C
			end
		end,
		0,
		A);
count2(T, A, K) ->
    [[H, V]] = ets:match(T, {K, '$1', '$2', '_'}),
    Anew = lists:foldl(fun({X, Y}, Ai) -> 
			       array2d:set(Ai,
					   1+array2d:get(Ai, X, Y), X, Y)
		       end,
		       A,
		       explode_rect(H, V)),
    count2(T, Anew, ets:next(T, K)).
    

main(File) ->
    T = get_claims(File),
    process_claims(T),
    [[ZeroId]] = ets:match(T, {'$1', '_', '_', 0}),
    {ZeroId, count2(T)}.
