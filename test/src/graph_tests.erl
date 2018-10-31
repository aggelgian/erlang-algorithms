-module(graph_tests).

-include_lib("eunit/include/eunit.hrl").

-spec test() -> 'ok' | {'error', term()}.  %% This should be provided by EUnit

-spec manual_test_() -> any().
manual_test_() ->
  G = graph:empty(undirected), %% Empty graph
  %% Add vertices
  lists:foreach(fun(V) -> graph:add_vertex(G, V) end, [athens, paris, london]),
  %% Add edges
  Es = [{athens, paris, 2096}, {athens, london, 2389}, {paris, london, 342}],
  lists:foreach(fun({F, T, W}) -> graph:add_edge(G, F, T, W) end, Es),
  graph:pprint(G),
  [{"xxx", ?_assertEqual(1, 1)}].
