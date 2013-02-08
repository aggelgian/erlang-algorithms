-module(doc).

-compile(export_all).

-spec make_doc() -> 'ok'.
make_doc() ->
  Mods = ["graph.erl", "heap.erl", "union_find.erl", "dfs.erl", "bfs.erl", "graph_lib.erl",
  "dijkstra.erl", "kruskal.erl"],
  Fs = lists:map(fun(M) -> filename:absname("src/" ++ M) end, Mods),
  edoc:files(Fs, [{dir, "doc"}]).
