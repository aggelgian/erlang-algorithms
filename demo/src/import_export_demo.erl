-module(import_export_demo).
-export([f/0]).

-spec f() -> true.
f() ->
  G = graph:empty(directed, d),
  Foo = graph:add_vertex(G, "foo"),
  Bar = graph:add_vertex(G, "bar"),
  Baz = graph:add_vertex(G, "baz"),
  _ = graph:add_edge(G, Foo, Bar, 20),
  _ = graph:add_edge(G, Bar, Foo, 20),
  _ = graph:add_edge(G, Bar, Baz, 80),
  Id = fun(V) -> V end,
  File = "graph.txt",
  graph:export(G, File, Id),
  G1 = graph:import(File, Id),
  true = graph:equal(G, G1).
