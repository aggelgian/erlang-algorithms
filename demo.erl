-module(demo).
-compile(export_all).

min_heaps() ->
  %% Creating a new min-heap
  H = heap:new(min),
  %% Checking if heap is empty
  true = heap:is_empty(H),
  %% Adding elements
  {6, _R1} = heap:insert(H, 6),
  {9, R2} = heap:insert(H, 9),
  {3, _R3} = heap:insert(H, 3),
  {12, _R4} = heap:insert(H, 12),
  %% Checking number of elements in the heap
  4 = heap:get_length(H),
  %% Finding the minimum element (without removing it)
  3 = heap:min(H),
  4 = heap:get_length(H),
  %% Removing the minimum element
  3 = heap:delete_head(H),
  3 = heap:get_length(H),
  %% Lowering the priority of an element
  true = heap:change(H, R2, 2),
  2 = heap:min(H),
  %% Increasing the priority of an element
  true = heap:change(H, R2, 20),
  6 = heap:min(H),
  %% Removing all the elements
  6 = heap:delete_head(H),
  12 = heap:delete_head(H),
  20 = heap:delete_head(H),
  true = heap:is_empty(H),
  %% Delete the heap
  true = heap:delete(H),
  %% Construct heap from a list
  L = [{1,6},{2,9},{3,3},{4,12}],
  {H2, _R} = heap:from_list(min, L),
  true = heap:delete(H2),
  demo_ok.
  
max_heaps() ->
  %% Creating a new max-heap
  H = heap:new(max),
  %% Checking if heap is empty
  true = heap:is_empty(H),
  %% Adding elements
  {6, _R1} = heap:insert(H, 6),
  {9, R2} = heap:insert(H, 9),
  {3, _R3} = heap:insert(H, 3),
  {12, _R4} = heap:insert(H, 12),
  %% Checking number of elements in the heap
  4 = heap:get_length(H),
  %% Finding the maximum element (without removing it)
  12 = heap:max(H),
  4 = heap:get_length(H),
  %% Removing the maximum element
  12 = heap:delete_head(H),
  3 = heap:get_length(H),
  %% Lowering the priority of an element
  true = heap:change(H, R2, 5),
  6 = heap:max(H),
  %% Increasing the priority of an element
  true = heap:change(H, R2, 20),
  20 = heap:max(H),
  %% Removing all the elements
  20 = heap:delete_head(H),
  6 = heap:delete_head(H),
  3 = heap:delete_head(H),
  true = heap:is_empty(H),
  %% Delete the heap
  true = heap:delete(H),
  %% Construct heap from a list
  L = [{1,6},{2,9},{3,3},{4,12}],
  {H2, _R} = heap:from_list(max, L),
  true = heap:delete(H2),
  demo_ok.
  
graph() ->
  X = 0,
  {ok, RootDir} = file:get_cwd(),
  File = RootDir ++ "/test_data/graph1.txt",
  G = graph:new_graph(File),
  Dijkstra = dijkstra:run(G, X),
  BFS = bfs:run(G,X),
  DFS = dfs:run(G,X),
  io:format("Dijkstra : ~p~n", [Dijkstra]),
  io:format("BFS : ~p~n", [BFS]),
  io:format("DFS : ~p~n", [DFS]).
