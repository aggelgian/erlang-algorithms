-module(dfs_demo).
-export([s1/0, s2/0]).

-spec s1() -> ok.
s1() ->
  Root = 0,
  G = graph_demo:g1(),
  DFS = dfs:run(G, Root),
  io:format("~p~n", [DFS]).

-spec s2() -> ok.
s2() ->
  Root = "a",
  G = graph_demo:g4(),
  DFS = dfs:run(G, Root),
  io:format("~p~n", [DFS]).
