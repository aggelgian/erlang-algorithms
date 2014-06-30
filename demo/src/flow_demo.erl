-module(flow_demo).
-export([s1/0]).

-spec s1() -> ok.

s1() ->
  G = graph_demo:g3(),
  Edmonds = edmonds_karp:run(G, 0, 5, bfs),
  Ford = edmonds_karp:run(G, 0, 5, dfs),
  io:format("Edmonds-Karp: ~p~n", [Edmonds]),
  io:format("Ford-Fulkerson: ~p~n", [Ford]),
  ok.

