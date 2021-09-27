-module(day07).
-export([parse_step/1,
	 get_dict/1,
	 get_roots/1,
	 iterate_graph/1,
	 calc_prereq/1]).

parse_step(Line) ->
    {ok, M} = 
	re:compile(
	  "Step (.) must be finished before step (.) can begin."),
    % by using [Pre] and [Post] I am implicitly saying that only one char
    % is being captured.
    {match, [[Pre], [Post]]} = re:run(Line, M, [{capture, 
						 all_but_first, list}]),
    {Pre, Post}.

get_dict(Lines) ->
    lists:foldl(
      fun (Line, Dict) ->
	      {Pre, Post} = parse_step(Line),
	      dict:update(Pre, 
			  fun (V) -> ordsets:add_element(Post, V) end,
			  ordsets:add_element(Post, ordsets:new()),
			  Dict)
      end,
      dict:new(), 
      Lines).

get_roots(G) ->
    undefined,
    {Pres, Posts } =
	dict:fold(fun (K, V, {Pres, Posts}) ->
			  {ordsets:add_element(K, Pres),
			   ordsets:union(V, Posts)}
		  end,
		  {ordsets:new(), ordsets:new()},
		  G),
    ordsets:subtract(Pres, Posts).

get_deps(G, H) ->
    case dict:find(H, G) of
	{ok, V} -> V;
	error -> [] % node is not requirement for any other node
    end.

calc_prereq(G) ->
    dict:fold(fun (K, V, A) ->
		      lists:foldl(fun (E, A1) ->
					  dict:update(E, 
						      fun (V1) ->
							      ordsets:add_element(K, V1)
						      end,
						      [K],
						      A1)
				  end,
				  A,
				  V)
	      end,
	      dict:new(),
	      G).

iterate_graph(_, _, Order, []) ->
    lists:reverse(Order);
iterate_graph({G, P}, Marked, Order, [H|T]) ->
    RD = lists:filter(
	   fun (D) ->
		   ordsets:is_subset(dict:fetch(D, P),
				     ordsets:add_element(H, Marked))
	   end,
	   get_deps(G, H)),
    iterate_graph({G, P}, 
		  ordsets:add_element(H, Marked),
		  [H|Order],
		  ordsets:union(RD, T)).
    
iterate_graph(G) ->
    iterate_graph({G, calc_prereq(G)}, [], [], get_roots(G)).

time_per_step(S) ->
    S - 4.
