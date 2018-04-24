-module(doc).
-export([make_doc/0]).

-spec make_doc() -> ok.
make_doc() ->
  Src = filename:absname("src"),
  Mods = [graph, heap, union_find, dfs, bfs, graph_lib, dijkstra, kruskal, edmonds_karp, a_star],
  Fs = [Src ++ "/" ++ atom_to_list(M) ++ ".erl" || M <- Mods],
  edoc:files(Fs, [{dir, "doc"}]).
