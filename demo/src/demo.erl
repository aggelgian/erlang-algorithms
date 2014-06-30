-module(demo).
-compile(export_all).
  
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

