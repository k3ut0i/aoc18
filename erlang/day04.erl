-module(day04).
-export([process_logs/1,
	 parse_log/1,
	 main/1]).
-import(util, [read_lines/1, freq_count/1]).

-define(TimeRegStr, "\\[(?<YEAR>[0-9]{4})-(?<MONTH>[0-9]{2})-(?<DAY>[0-9]{2}) (?<HOUR>[0-9]{2}):(?<MIN>[0-9]{2})\\]").
-define(GuardShift, "(?<G>Guard \\#(?<ID>[0-9]+) begins shift)").
-define(GuardAsleep, "(?<A>falls asleep)").
-define(GuardWakes, "(?<W>wakes up)").

parse_log(S) when is_list(S) ->
    {ok, Rs} = re:compile(?TimeRegStr ++ " (" ++
			      ?GuardShift ++ "|" ++
			      ?GuardAsleep ++ "|" ++
			      ?GuardWakes ++ ")"),
    case re:run(S, Rs, [{capture, all_names, list}]) of 
	{match, L} ->
	    {namelist, Ns} = re:inspect(Rs, namelist),
	    D = dict:from_list(lists:zip(Ns, L)),
	    T = {erlang:list_to_integer(dict:fetch(<<"YEAR">>, D)),
		 erlang:list_to_integer(dict:fetch(<<"MONTH">>, D)),
		 erlang:list_to_integer(dict:fetch(<<"DAY">>, D)),
		 erlang:list_to_integer(dict:fetch(<<"HOUR">>, D)),
		 erlang:list_to_integer(dict:fetch(<<"MIN">>, D))},
	    case {dict:fetch(<<"G">>, D), dict:fetch(<<"A">>, D), dict:fetch(<<"W">>, D)} of
		{_, [], []} ->
		    {T, erlang:list_to_integer(dict:fetch(<<"ID">>, D))};
		{[], _, []} ->
		    {T, asleep};
		{[], [], _} ->
		    {T, awake}
	    end;
	nomatch ->
	    throw("Could not parse: " ++ S)
    end.
%% {match, L} = re:run(S, Rs).
dict_order([]) ->
    false;
dict_order([{P1, P2} | Rest]) ->
    if P1 < P2 ->
	    true;
       P1 > P2 ->
	    false;
       P1 == P2 ->
	    dict_order(Rest)
    end.

time_order({Y1, M1, D1, H1, Mi1}, {Y2, M2, D2, H2, Mi2}) ->
    dict_order([{Y1, Y2}, {M1, M2}, {D1, D2}, {H1, H2}, {Mi1, Mi2}]).

partition_logs(Ls) ->
    partition_logs(Ls, []).
partition_logs([], Acc) ->
    [lists:reverse(Acc)];
partition_logs([L | Ls], Acc)  ->
    case L of
	{_, awake} ->
	    partition_logs(Ls, [L|Acc]);
	{_, asleep} ->
	    partition_logs(Ls, [L|Acc]);
	{_, N} when is_integer(N) ->
	    [lists:reverse(Acc) | partition_logs(Ls, [L])]
    end.


process_logs(Ls) ->
    SortedLogs = lists:sort(fun ({T1, _}, {T2, _}) ->
				    time_order(T1, T2)
			    end,
			    lists:map(fun parse_log/1, Ls)),
    partition_logs(SortedLogs).

%% XXX: very constrained assumption that works with this data
%% since only midnight hour is relevant here
time_diff({Y, M, D, H, Mi1}, {Y, M, D, H, Mi2}) ->
    lists:seq(Mi1, Mi2-1).


count_min([{_, Id}| R]) when is_integer(Id) -> %% aggregate asleep minutes in each partition
    {Id, count_min_r(R)}.
count_min_r([]) -> [];
count_min_r([{T1, asleep}, {T2, awake} | R]) -> 
    time_diff(T1, T2) ++ count_min_r(R).

asleep_count(Ls) ->
    asleep_count(Ls, dict:new()).
asleep_count([], D) ->
    D;
%% asleep_count([], D) ->
%%     dict:fold(fun (Id, Time, {IdMax, TimeMax}) ->
%% 		      if length(Time) > length(TimeMax) ->
%% 			      {Id, Time};
%% 			 length(Time) =< length(TimeMax) ->
%% 			      {IdMax, TimeMax}
%% 		      end
%% 	      end, 
%% 	      {undefined, []}, 
%% 	      D);
asleep_count([L|Ls], D) ->
    {Id, Min} = count_min(L),
    Dn = dict:update(Id, fun (X) -> X ++ Min end, Min, D),
    asleep_count(Ls, Dn).

max_val(D) ->
    dict:fold(fun (Min, Count, {MaxMin, MaxCount}) ->
		      if Count > MaxCount -> 
			      {Min, Count};
			 Count =< MaxCount ->
			      {MaxMin, MaxCount}
		      end
	      end,
	      {undefined, 0},
	      D).

main(File) ->
    Ls = lists:filter(fun (E) -> E /= [] end,
		      process_logs(util:read_lines(File))),
    AC = asleep_count(Ls),
    {Id1, Mins1} = dict:fold(fun (Id, Time, {IdMax, TimeMax}) ->
				     if length(Time) > length(TimeMax) ->
					     {Id, Time};
					length(Time) =< length(TimeMax) ->
					     {IdMax, TimeMax}
				     end
			     end, 
			     {undefined, []}, 
			     AC),
    {MaxMin1, _} = max_val(util:freq_count(Mins1)),
    {Id2, MaxMin2, _} =
	dict:fold(fun (Id, Times, {IdMax, MinMax, CountMax}) -> 
			  {TimeMax, Count} =
			      max_val(util:freq_count(Times)),
			  if Count > CountMax ->
				  {Id, TimeMax, Count};
			     Count =< CountMax ->
				  {IdMax, MinMax, CountMax}
			  end
		  end,
		  {undefined, undefined, 0},
		  AC),
    {{Id1, MaxMin1}, {Id2, MaxMin2}}.
