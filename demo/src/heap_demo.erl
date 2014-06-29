-module(heap_demo).
-export([min_heap/0, max_heap/0]).

-spec min_heap() -> ok.

min_heap() ->
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
  4 = heap:heap_size(H),
  %% Finding the minimum element (without removing it)
  3 = heap:min(H),
  4 = heap:heap_size(H),
  %% Removing the minimum element
  3 = heap:take_min(H),
  3 = heap:heap_size(H),
  %% Lowering the priority of an element
  true = heap:update(H, R2, 2),
  2 = heap:min(H),
  %% Increasing the priority of an element
  true = heap:update(H, R2, 20),
  6 = heap:min(H),
  %% Removing all the elements
  6 = heap:take_min(H),
  12 = heap:take_min(H),
  20 = heap:take_min(H),
  true = heap:is_empty(H),
  %% Delete the heap
  true = heap:delete(H),
  %% Construct heap from a list
  L = [{1,6},{2,9},{3,3},{4,12}],
  {H2, _R} = heap:from_list(min, L),
  true = heap:delete(H2),
  ok.

-spec max_heap() -> ok.

max_heap() ->
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
  4 = heap:heap_size(H),
  %% Finding the maximum element (without removing it)
  12 = heap:max(H),
  4 = heap:heap_size(H),
  %% Removing the maximum element
  12 = heap:take_max(H),
  3 = heap:heap_size(H),
  %% Lowering the priority of an element
  true = heap:update(H, R2, 5),
  6 = heap:max(H),
  %% Increasing the priority of an element
  true = heap:update(H, R2, 20),
  20 = heap:max(H),
  %% Removing all the elements
  20 = heap:take_max(H),
  6 = heap:take_max(H),
  3 = heap:take_max(H),
  true = heap:is_empty(H),
  %% Delete the heap
  true = heap:delete(H),
  %% Construct heap from a list
  L = [{1,6},{2,9},{3,3},{4,12}],
  {H2, _R} = heap:from_list(max, L),
  true = heap:delete(H2),
  ok.
