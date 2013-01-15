-module(dijkstra).
-export([run/3]).

%% Testcase
%% --------
%% dijkstra:run(11,[{0,1,4},{0,4,1},{0,6,2},{1,2,7},{1,8,2},{2,3,8},{4,5,7},{4,6,5},{6,7,1},{7,8,6},{7,9,4},{8,10,3},{9,10,2}],0).

%% Dijkstra's Algorithm
%% Assumes an undirected connected graph
%%
%% Choice of Data Structures
%% -------------------------
%% Graph        digraph()
%% Open Set     gb_tree()
%% Closed Set   dict()
%% Result       dict()

%% Graph Abstractions
-define(EMPTY_GRAPH, digraph:new([cyclic])).
-define(ADD_VERTEX(Graph, Vertex), digraph:add_vertex(Graph, Vertex)).
-define(ADD_EDGE(From, To, Weight, Graph), digraph:add_edge(Graph, {From, To}, From, To, Weight)).
-define(EDGE_WEIGHT(Graph, Edge), element(4, digraph:edge(Graph, Edge))).
-define(GET_NEIGHBOURS(Vertex, Graph), digraph:out_neighbours(Graph, Vertex)).
%% Heap (Open Set) Abstractions
-define(IS_EMPTY_HEAP(Heap), gb_trees:is_empty(Heap)).
-define(EMPTY_HEAP, gb_trees:empty()).
-define(INSERT_NODE_TO_HEAP(Node, Prev, Cost, Heap), gb_trees:insert({Cost, Node, Prev}, 0, Heap)).
-define(FILTER_MIN_HEAP(R), erlang:append_element(element(1, R), element(3, R))).
-define(GET_MIN_HEAP(Heap), ?FILTER_MIN_HEAP(gb_trees:take_smallest(Heap))).
%% Visited (Closed Set) Abstractions
-define(EMPTY_VISITED, dict:new()).
-define(IS_VISITED(V, Visited), dict:is_key(V, Visited)).
-define(ADD_TO_VISITED(Node, OldVisited), dict:store(Node, true, OldVisited)).
%% Result Abstractions
-define(EMPTY_RESULT, dict:new()).
-define(ADD_TO_RESULT(Node, Cost, Prev, Result), dict:store(Node, {Cost, Prev}, Result)).
-define(RETURN_RESULT(Result), dict:to_list(Result)).

%% ----------------------------------------------------------
%% Exported Functions
%% ----------------------------------------------------------

%% ----------------------------------------------------------
%% run(Vertices, Edges, Root) -> Result
%%   Vertices :: Number of graph's vertices // 0..N-1
%%   Edges    :: List of graph's edges // [{From, To, Weight}]
%%   Result   :: [{Node, {Cost, Prev}}]
%% ----------------------------------------------------------
run(Vertices, Edges, Root) ->
  Graph = init_graph(Vertices, Edges),
  dijkstra(Graph, Root).

%% ----------------------------------------------------------
%% Initialize Graph
%% ----------------------------------------------------------

init_graph(N, E) ->
  Graph = ?EMPTY_GRAPH,
  add_vertices(N, Graph),
  add_edges(E, Graph),
  Graph.
  
add_vertices(N, Graph) -> add_vertices(N, Graph, 0).

add_vertices(_N, Graph, _N) ->
  Graph;
add_vertices(N, Graph, Vertex) ->
  ?ADD_VERTEX(Graph, Vertex),
  add_vertices(N, Graph, Vertex+1).
  
add_edges([], Graph) ->
  Graph;
add_edges([{From, To, Weight}|Es], Graph) ->
  ?ADD_EDGE(From, To, Weight, Graph),
  ?ADD_EDGE(To, From, Weight, Graph),
  add_edges(Es, Graph).
  
%% ---------------------------------------------------------
%% Dijkstra Functions
%% ---------------------------------------------------------

%% Wrapper function
dijkstra(Graph, Root) ->
  {Heap, Visited, Result} = dijkstra_init(Root),
  dijkstra_step(Graph, Heap, Visited, Result).
  
%% Initalize Heap (Open Set), Visited (Closed Set), Result
dijkstra_init(Root) ->
  Heap = ?INSERT_NODE_TO_HEAP(Root, root, 0, ?EMPTY_HEAP),
  Visited = ?EMPTY_VISITED,
  Result = ?EMPTY_RESULT,
  {Heap, Visited, Result}.
  
%% Dijkstra loop
dijkstra_step(Graph, Heap, Visited, Result) ->
  case ?IS_EMPTY_HEAP(Heap) of
    true ->
      ?RETURN_RESULT(Result);
    false ->
      {Cost, Node, Prev, NewHeap} = ?GET_MIN_HEAP(Heap),
      case ?IS_VISITED(Node, Visited) of
        true ->
          dijkstra_step(Graph, NewHeap, Visited, Result);
        false ->
          NewResult = ?ADD_TO_RESULT(Node, Cost, Prev, Result),
          NewVisited = ?ADD_TO_VISITED(Node, Visited),
          AdjList = ?GET_NEIGHBOURS(Node, Graph),
          NextHeap = 
            lists:foldl(
              fun(V,  H) ->
                case ?IS_VISITED(V, NewVisited) of
                  true ->
                    H;
                  false ->
                    Edge = {Node, V},
                    Weight = ?EDGE_WEIGHT(Graph, Edge),
                    ?INSERT_NODE_TO_HEAP(V, Node, Cost + Weight, H)
                end
              end,
              NewHeap, AdjList
            ),
          dijkstra_step(Graph, NextHeap, NewVisited, NewResult)
    end
  end.
