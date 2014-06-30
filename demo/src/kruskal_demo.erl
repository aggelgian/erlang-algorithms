-module(kruskal_demo).
-export([s1/0, s2/0]).

-spec s1() -> ok.
s1() ->
  G = graph_demo:g1(),
  Kruskal = kruskal:run(G),
  io:format("~p~n", [Kruskal]).

-spec s2() -> ok.
s2() ->
  G = graph_demo:g4(),
  Kruskal = kruskal:run(G),
  io:format("~p~n", [Kruskal]).
