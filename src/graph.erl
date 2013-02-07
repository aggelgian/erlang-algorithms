%%
%% %CopyrightBegin%
%%
%% Copyright © 2013 Aggelos Giantsios
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy of this software 
%% and associated documentation files (the “Software”), to deal in the Software without restriction, 
%% including without limitation the rights to use, copy, modify, merge, publish, distribute, 
%% sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is 
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included 
%% in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED 
%% TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
%% IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN 
%% CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
%%
%% %CopyrightEnd%
%%

%% @copyright 2013 Aggelos Giantsios
%% @author Aggelos Giantsios

%% ============================================================================
%% @doc Directed / Undirected Graphs
%%
%% <p>This module implements directed and undirected graphs that are either 
%% weighted or unweighted.</p>
%%
%% <p>It is basically syntactic sugar for the digraph module with added 
%% support for undirected graphs.</p>
%%
%% <h3>How to use</h3>
%% <p>In order to create a graph you must load it from a file.
%% The file that contains the graph must have the following format.</p>
%% 
%% <ul>
%%   <li>The 1st line will consist of four terms separeted by a white space.
%%     <ul>
%%       <li>1st Term: Positive Integer N that denotes the number of vertices</li>
%%       <li>2nd Term: Positive Integer M that denotes the number of edges.</li>
%%       <li>3rd Term: Atom <code>directed</code> or <code>undirected</code> that denotes the type of the graph.</li>
%%       <li>4th Term: Atom <code>unweighted</code> or <code>d</code> or <code>f</code> that denotes the type of the edge weights.
%%         <ul>
%%           <li><code>unweighted</code> is for an unweighted graph.</li>
%%           <li><code>d</code> is for decimal integer weights.</li>
%%           <li><code>f</code> is for floating point number weights in proper Erlang syntax.</li>
%%         </ul>
%%       </li>
%%     </ul>
%%   </li>
%%   <li>The next M lines will consist of the edge descriptions. 
%%       Each line will contain three terms : U V W. 
%%       This will denote an edge from U to V with W weight.</li>
%% </ul>
%% 
%% <p>For examples you can check the files in the test_data directory.</p>
%% 

-module(graph).

-export([new_graph/1, del_graph/1, vertices/1, edges/1, edge_weight/2,
         edges_with_weights/1, out_neighbours/2, num_of_vertices/1,
         num_of_edges/1, pprint/1]).

-export_type([graph/0, vertex/0, edge/0]).

%%
%% @type graph(). A directed or undirected graph.
%% <p>It is wrapper for a digraph with the extra information on its type.</p>
%%
-record(graph, {type :: graphtype(), graph :: digraph()}).
-opaque graph()  :: #graph{}.
-type vertex() :: non_neg_integer().
-type edge()   :: {vertex(), vertex()}.
-type graphtype()  :: 'directed' | 'undirected'.
-type weighttype() :: 'unweighted' | 'd' | 'f'.

%% ==========================================================
%% Exported Functions
%% ==========================================================

%% @doc Create a new graph from a file
-spec new_graph(file:name()) -> graph().

new_graph(File) ->
  {'ok', IO} = file:open(File, ['read']),
  %% N = Number of Vertices :: non_neg_integer()
  %% M = Number of Edges :: non_neg_integer()
  %% T = Graph Type :: directed | undirected
  %% W = Edge Weight weighted :: Weight Type (d | f) | unweighted
  {'ok', [N, M, T, W]} = io:fread(IO, ">", "~d ~d ~a ~a"),
  G = digraph:new(),
  %% 3rd Argument is the number of the first vertex
  'ok' = init_vertices(G, N, 0),
  'ok' = init_edges(G, M, IO, T, W),
  #graph{type=T, graph=G}.
  
%% @doc Delete a graph
-spec del_graph(graph()) -> 'true'.
  
del_graph(G) ->
  digraph:delete(G#graph.graph).
  
%% @doc Return a list of the vertices of a graph
-spec vertices(graph()) -> [vertex()].
  
vertices(G) ->
  digraph:vertices(G#graph.graph).

%% @doc Return the number of vertices in a graph
-spec num_of_vertices(graph()) -> non_neg_integer().
  
num_of_vertices(G) ->
  Vs = vertices(G),
  length(Vs).
  
%% @doc Return a list of the edges of a graph
-spec edges(graph()) -> [edge()].
  
edges(G) ->
  Es = digraph:edges(G#graph.graph),
  case G#graph.type of
    'directed' ->
      Es;
    'undirected' ->
      remove_duplicate_edges(Es, [])
  end.
  
%% @doc Return the number of edges in a graph
-spec num_of_edges(graph()) -> non_neg_integer().
  
num_of_edges(G) ->
  Es = edges(G),
  length(Es).
  
%% @doc Return the weight of an edge
-spec edge_weight(graph(), edge()) -> term().
  
edge_weight(G, E) ->
  {E, _V1, _V2, W} = digraph:edge(G#graph.graph, E),
  W.
  
%% @doc Return a list of the edges of a graph along with their weights
-spec edges_with_weights(graph()) -> [{edge(), term()}].
  
edges_with_weights(G) ->
  Es = edges(G),
  lists:map(fun(E) -> {E, edge_weight(G, E)} end, Es).
  
%% @doc Return a list of the out neighbours of a vertex
-spec out_neighbours(graph(), vertex()) -> [vertex()].
  
out_neighbours(G, V) ->
  digraph:out_neighbours(G#graph.graph, V).
  
%% @doc Pretty print a graph
-spec pprint(graph()) -> 'ok'.
  
pprint(G) ->
  Vs = digraph:vertices(G#graph.graph),
  F = 
    fun(V) ->
      Es = digraph:out_edges(G#graph.graph, V),
      Ns = lists:map(
        fun(E) -> 
          {E, _V1, V2, W} = digraph:edge(G#graph.graph, E),
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

%% Initialize the vertices of the graph
-spec init_vertices(digraph(), non_neg_integer(), non_neg_integer()) -> 'ok'.
  
init_vertices(_G, _N, _N) ->
  ok;
init_vertices(G, N, V) ->
  digraph:add_vertex(G, V),
  init_vertices(G, N, V+1).
  
%% Initialize the edges of the graph
-spec init_edges(digraph(), non_neg_integer(), file:io_device(), graphtype(), weighttype()) -> 'ok'.
  
init_edges(_G, 0, _IO, _T, _WT) ->
  'ok';
init_edges(G, M, IO, T, 'unweighted') ->
  {'ok', [V1, V2]} = io:fread(IO, ">", "~d ~d"),
  case T of
    directed ->
      digraph:add_edge(G, {V1, V2}, V1, V2, 1);
    undirected ->
      digraph:add_edge(G, {V1, V2}, V1, V2, 1),
      digraph:add_edge(G, {V2, V1}, V2, V1, 1)
  end,
  init_edges(G, M-1, IO, T, 'unweighted');
init_edges(G, M, IO, T, WT) ->
  Format = "~d ~d ~" ++ atom_to_list(WT),
  {'ok', [V1, V2, W]} = io:fread(IO, ">", Format),
  case T of
    directed ->
      digraph:add_edge(G, {V1, V2}, V1, V2, W);
    undirected ->
      digraph:add_edge(G, {V1, V2}, V1, V2, W),
      digraph:add_edge(G, {V2, V1}, V2, V1, W)
  end,
  init_edges(G, M-1, IO, T, WT).
  
-spec remove_duplicate_edges([edge()], [edge()]) -> [edge()].
remove_duplicate_edges([], Acc) ->
  Acc;
remove_duplicate_edges([{From, To}=E|Es], Acc) ->
  remove_duplicate_edges(Es -- [{To, From}], [E|Acc]).
  
