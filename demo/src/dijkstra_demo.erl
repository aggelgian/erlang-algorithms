-module(dijkstra_demo).
-export([s1/0, s2/0]).

-spec s1() -> ok.
s1() ->
  Root = 0,
  G = graph_demo:g1(),
  Dijkstra = dijkstra:run(G, Root),
  io:format("Root : ~p~n~p~n", [Root, Dijkstra]).

-spec s2() -> ok.
s2() ->
  Root = "a",
  G = graph_demo:g4(),
  Dijkstra = dijkstra:run(G, Root),
  io:format("Root : ~p~n~p~n", [Root, Dijkstra]).
