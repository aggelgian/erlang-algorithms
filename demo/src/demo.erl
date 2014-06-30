-module(demo).
-compile(export_all).

-spec graph() -> 'true'.
graph() ->
  X = 0,
  {'ok', RootDir} = file:get_cwd(),
  File = RootDir ++ "/demo/data/graph1.txt",
  G = graph:from_file(File),
  Kruskal = kruskal:run(G),
  io:format("Kruskal : ~p~n", [Kruskal]),
  erlang:display('demo_ok').
  
-spec flow() -> 'true'.
flow() ->
  {'ok', RootDir} = file:get_cwd(),
  File = RootDir ++ "/demo/data/graph3.txt",
  G = graph:from_file(File),
  Edmonds = edmonds_karp:run(G, 0, 5, 'bfs'),
  Ford = edmonds_karp:run(G, 0, 5, 'dfs'),
  io:format("Edmonds-Karp: ~p~n", [Edmonds]),
  io:format("Ford-Fulkerson: ~p~n", [Ford]),
  erlang:display('demo_ok').

