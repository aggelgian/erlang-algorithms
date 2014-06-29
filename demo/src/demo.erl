-module(demo).
-compile(export_all).

-spec graph() -> 'true'.
graph() ->
  X = 0,
  {'ok', RootDir} = file:get_cwd(),
  File = RootDir ++ "/demo/data/graph1.txt",
  G = graph:from_file(File),
  Dijkstra = dijkstra:run(G, X),
  BFS = bfs:run(G,X),
  DFS = dfs:run(G,X),
  Kruskal = kruskal:run(G),
  io:format("Dijkstra : ~p~n", [Dijkstra]),
  io:format("BFS : ~p~n", [BFS]),
  io:format("DFS : ~p~n", [DFS]),
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
  
-spec union_find() -> 'true'.
union_find() ->
  L = [a,b,c,d,e,f,g,h,i,j],
  F = union_find:singletons_from_list(L),
%%  union_find:pprint(F),
  10 = union_find:number_of_sets(F),
  'true' = union_find:union(F, a, e),
%%  union_find:pprint(F),
  'true' = union_find:union(F, a, d),
%%  union_find:pprint(F),
  'true' = union_find:union(F, g, i),
%%  union_find:pprint(F),
  'true' = union_find:union(F, h, f),
%%  union_find:pprint(F),
  6 = union_find:number_of_sets(F),
  [a,d,e] = lists:sort(union_find:set_elements(F, e)),
  g = union_find:find(F, i),
  3 = union_find:set_size(F, e),
  'true' = union_find:delete(F),
  erlang:display('demo_ok').
  
