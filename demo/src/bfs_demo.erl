-module(bfs_demo).
-export([s1/0, s2/0]).

-spec s1() -> ok.
s1() ->
  Root = 0,
  G = graph_demo:g1(),
  BFS = bfs:run(G, Root),
  io:format("~p~n", [BFS]).

-spec s2() -> ok.
s2() ->
  Root = "a",
  G = graph_demo:g4(),
  BFS = bfs:run(G, Root),
  io:format("~p~n", [BFS]).
