-module(day02).
-export([part1/1, part2/1,
	 differ_one_char/2]).
-import(util, [read_lines/1,
	       search_pair/2]).

count_chars([]) ->
    dict:new();
count_chars([C|Cs]) ->
    dict:update(C, fun(X) -> X+1 end, 1, count_chars(Cs)).
    
find_keys(F, D) ->
    dict:fetch_keys(dict:filter(F, D)).

boolc(true) ->
    1;
boolc(false) ->
    0.
req(Str) ->
    C = count_chars(Str),
    Twos = find_keys(fun (_, V) -> V == 2 end, C), % XXX: we can find both with one iteration
    Threes = find_keys(fun (_, V) -> V == 3 end, C),
    {boolc(length(Twos) > 0), boolc(length(Threes) > 0)}.

checksum(Ss) ->
    lists:foldl(fun(S, {A2, A3}) -> 
			{D2, D3} = req(S), 
			{A2+D2, A3+D3}
		end,
		{0, 0}, Ss).
part1(F) ->
    checksum(util:read_lines(F)).

differ_one_char([], _) ->
    false;
differ_one_char(_, []) ->
    false;
differ_one_char([A|As], [B|Bs]) ->
    case A == B of
       true ->
	    differ_one_char(As, Bs);
       false -> As == Bs
    end.

part2(F) ->
    search_pair(fun differ_one_char/2, util:read_lines(F)).
