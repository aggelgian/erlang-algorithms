%%
%% Copyright © 2013 Aggelos Giantsios
%%

%% Permission is hereby granted, free of charge, to any person obtaining a copy of this software 
%% and associated documentation files (the “Software”), to deal in the Software without restriction, 
%% including without limitation the rights to use, copy, modify, merge, publish, distribute, 
%% sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is 
%% furnished to do so, subject to the following conditions:

%% The above copyright notice and this permission notice shall be included 
%% in all copies or substantial portions of the Software.

%% THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED 
%% TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
%% IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN 
%% CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

%%
%% Directed / Undirected Graphs
%%

%% This module implements directed and undirected graphs that are either weighted or unweighted.
%% It is basically syntactic sugar for the digraph module with added support for undirected graphs.

-module(graph).
-export([new_graph/1, del_graph/1, vertices/1, edges/1, edge_weight/2,
  out_neighbours/2, pprint/1]).

-export_type([graph/0, vertex/0, edge/0]).

-type graph() :: digraph().
-type vertex() :: non_neg_integer() | atom().
-type edge() :: {vertex(), vertex()}.
-type graphtype() :: directed | undirected.
-type weighttype() :: unweighted | d | f.

%% ==========================================================
%% Exported Functions
%% ==========================================================

-spec new_graph(File) -> Graph
  when File :: file:filename(),
       Graph :: graph().

new_graph(File) ->
  {ok, IO} = file:open(File, [read]),
  %% N = Number of Vertices :: non_neg_integer()
  %% M = Number of Edges :: non_neg_integer()
  %% T = Graph Type :: directed | undirected
  %% W = Edge Weight weighted :: Weight Type (see fread sequences) | unweighted
  {ok, [N, M, T, W]} = io:fread(IO, ">", "~d ~d ~a ~a"),
  G = digraph:new(),
  %% 3rd Argument is the number of the first vertex
  ok = init_vertices(G, N, 0),
  ok = init_edges(G, M, IO, T, W),
  G.
  
-spec del_graph(Graph) -> true
  when Graph :: graph().
  
del_graph(G) ->
  digraph:delete(G).
  
-spec vertices(Graph) -> Vertices
  when Graph :: graph(),
       Vertices :: [vertex()].
       
vertices(G) ->
  digraph:vertices(G).
  
-spec edges(Graph) -> Edges
  when Graph :: graph(),
       Edges :: [edge()].
       
edges(G) ->
  digraph:edges(G).
  
-spec edge_weight(Graph, Edge) -> Weight
  when Graph :: graph(),
       Edge :: edge(),
       Weight :: term().
       
edge_weight(G, E) ->
  {E, _V1, _V2, W} = digraph:edge(G, E),
  W.
  
-spec out_neighbours(Graph, Node) -> Neighbours
  when Graph :: graph(),
       Node :: vertex(),
       Neighbours :: [vertex()].
       
out_neighbours(G, V) ->
  digraph:out_neighbours(G, V).
  
-spec pprint(Graph) -> ok
  when Graph :: graph().
  
pprint(G) ->
  Vs = digraph:vertices(G),
  F = 
    fun(V) ->
      Es = digraph:out_edges(G, V),
      Ns = lists:map(
        fun(E) -> 
          {E, _V1, V2, W} = digraph:edge(G, E),
          {V2, W}
        end, 
        Es),
      {V, Ns}
    end,
  N = lists:sort(fun erlang:'<'/2, lists:map(F, Vs)),
  io:format("[{From, [{To, Weight}]}]~n"),
  io:format("========================~n"),
  io:format("~p~n", [N]).
  
%% ==========================================================
%% Internal Functions
%% ==========================================================

-spec init_vertices(Graph, N, V) -> ok
  when Graph :: graph(),
       N :: non_neg_integer(),
       V :: non_neg_integer().
       
init_vertices(_G, _N, _N) ->
  ok;
init_vertices(G, N, V) ->
  digraph:add_vertex(G, V),
  init_vertices(G, N, V+1).
  
-spec init_edges(Graph, M, IO, GraphType, WeightType) -> ok
  when Graph :: graph(),
       M :: non_neg_integer(),
       IO :: io:device(),
       GraphType :: graphtype(),
       WeightType :: weighttype().
  
init_edges(_G, 0, _IO, _T, _WT) ->
  ok;
init_edges(G, M, IO, T, unweighted) ->
  {ok, [V1, V2]} = io:fread(IO, ">", "~d ~d"),
  case T of
    directed ->
      digraph:add_edge(G, {V1, V2}, V1, V2, 1);
    undirected ->
      digraph:add_edge(G, {V1, V2}, V1, V2, 1),
      digraph:add_edge(G, {V2, V1}, V2, V1, 1)
  end,
  init_edges(G, M-1, IO, T, unweighted);
init_edges(G, M, IO, T, WT) ->
  Format = "~d ~d ~" ++ atom_to_list(WT),
  {ok, [V1, V2, W]} = io:fread(IO, ">", Format),
  case T of
    directed ->
      digraph:add_edge(G, {V1, V2}, V1, V2, W);
    undirected ->
      digraph:add_edge(G, {V1, V2}, V1, V2, W),
      digraph:add_edge(G, {V2, V1}, V2, V1, W)
  end,
  init_edges(G, M-1, IO, T, WT).
