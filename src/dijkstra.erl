%% @doc Dijkstra's Algorithm
%% 
%% <p>For examples you can check the <code>dijsktra_demo</code> module.</p>
%% 

-module(dijkstra).

-export([run/2]).

-type open_set()   :: gb_trees:tree().
-type closed_set() :: dict:dict().
-type parents()    :: dict:dict().

%% ----------------------------------------------------------
%% Dijkstra Abstractions
%% ----------------------------------------------------------

%% Graph Abstractions
-define(EDGE_WEIGHT(Graph, Edge), graph:edge_weight(Graph, Edge)).
-define(GET_NEIGHBOURS(Vertex, Graph), graph:out_neighbours(Graph, Vertex)).
%% Heap (Open Set) Abstractions
-define(IS_EMPTY_HEAP(Heap), gb_trees:is_empty(Heap)).
-define(EMPTY_HEAP, gb_trees:empty()).
-define(INSERT_NODE_TO_HEAP(Node, Prev, Cost, Heap), gb_trees:insert({Cost, Node, Prev}, 0, Heap)).
-define(FILTER_MIN_HEAP(R), erlang:append_element(element(1, R), element(3, R))).
-define(GET_MIN_HEAP(Heap), ?FILTER_MIN_HEAP(gb_trees:take_smallest(Heap))).
%% Visited (Closed Set) Abstractions
-define(EMPTY_VISITED, dict:new()).
-define(IS_VISITED(V, Visited), dict:is_key(V, Visited)).
-define(ADD_TO_VISITED(Node, OldVisited), dict:store(Node, 'true', OldVisited)).
%% Parents Abstractions
-define(EMPTY_PARENTS, dict:new()).
-define(ADD_TO_PARENTS(Node, Cost, Prev, P), dict:store(Node, {Cost, Prev}, P)).

%% ==========================================================
%% Exported Functions
%% ==========================================================

%% @doc Runs the Dijkstra algorithm on a graph <code>Graph</code>
%% with <code>Root</code> as point of origin
-spec run(graph:graph(), graph:vertex()) -> [graph_lib:path_info()].
run(Graph, Root) ->
  {Heap, Visited, Parents} = dijkstra_init(Root),
  R = dijkstra_step(Graph, Heap, Visited, Parents),
  Vertices = graph:vertices(Graph),
  graph_lib:reconstruct_all_paths(Vertices, R).

%% ==========================================================
%% Dijkstra Functions
%% ==========================================================

%% Initalize Heap (Open Set), Visited (Closed Set), Parents
-spec dijkstra_init(graph:vertex()) -> {open_set(), closed_set(), parents()}.
dijkstra_init(Root) ->
  Heap = ?INSERT_NODE_TO_HEAP(Root, root, 0, ?EMPTY_HEAP),
  Visited = ?EMPTY_VISITED,
  Parents = ?EMPTY_PARENTS,
  {Heap, Visited, Parents}.

%% Dijkstra loop
-spec dijkstra_step(graph:graph(), open_set(), closed_set(), parents()) -> parents().
dijkstra_step(Graph, Heap, Visited, Parents) ->
  case ?IS_EMPTY_HEAP(Heap) of
    true ->
      Parents;
    false ->
      {Cost, Node, Prev, NewHeap} = ?GET_MIN_HEAP(Heap),
      case ?IS_VISITED(Node, Visited) of
        true ->
          dijkstra_step(Graph, NewHeap, Visited, Parents);
        false ->
          NewParents = ?ADD_TO_PARENTS(Node, Cost, Prev, Parents),
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
          dijkstra_step(Graph, NextHeap, NewVisited, NewParents)
    end
  end.

