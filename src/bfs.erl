%%
%% %CopyrightBegin%
%%
%% Copyright © 2013-2014 Aggelos Giantsios
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

%% @copyright 2013-2014 Aggelos Giantsios
%% @author Aggelos Giantsios

%% ============================================================================
%% @doc BFS Algorithm
%%

-module(bfs).

-export([run/2]).

-type states()  :: dict:dict().
-type parents() :: dict:dict().

%% ----------------------------------------------------------
%% BFS Abstractions
%% ----------------------------------------------------------

%% Queue Abstraction
-define(EMPTY_QUEUE(), queue:new()).
-define(IS_EMPTY(Q), queue:is_empty(Q)).
-define(ADD_TO_QUEUE(Node, Cost, Q), queue:in({Node, Cost}, Q)).
-define(FILTER_EXTRACT(R), {erlang:element(2, erlang:element(1, R)), erlang:element(2, R)}).
-define(EXTRACT_FROM_QUEUE(Q), ?FILTER_EXTRACT(queue:out(Q))).
%% States Abstractions
%% State 'A' : not visited
%% State 'Y' : explored but not added to the result set
%% State 'E' : explored and added to result set
-define(EMPTY_STATES(), dict:new()).
-define(SET_STATE(Node, State, States), dict:store(Node, State, States)).
-define(GET_STATE(Node, States), dict:fetch(Node, States)).
%% Parents Abstractions
-define(EMPTY_PARENTS(), dict:new()).
-define(ADD_TO_PARENTS(Node, Cost, Prev, R), dict:store(Node, {Cost, Prev}, R)).

%% ==========================================================
%% Exported Functions
%% ==========================================================

%% @doc Runs the BFS algorithm on a graph <code>Graph</code>
%% with <code>Root</code> as point of origin
-spec run(graph:graph(), graph:vertex()) -> [graph_lib:path_info()].
run(Graph, Root) ->
  {Q, M, P} = bfs_init(Graph, Root),
  Result = bfs_step(Graph, Q, M, P),
  Vertices = graph:vertices(Graph),
  graph_lib:reconstruct_all_paths(Vertices, Result).

%% ==========================================================
%% BFS Functions
%% ==========================================================

%% Initialize data structures
-spec bfs_init(graph:graph(), graph:vertex()) -> {queue:queue(), states(), parents()}.
bfs_init(Graph, Root) ->
  EQ = ?EMPTY_QUEUE(),
  EM = ?EMPTY_STATES(),
  EP = ?EMPTY_PARENTS(),
  NQ = ?ADD_TO_QUEUE(Root, 0, EQ),
  NM = ?SET_STATE(Root, 'Y', EM),
  NP = ?ADD_TO_PARENTS(Root, 0, 'root', EP),
  Vs = graph:vertices(Graph) -- [Root],
  NxtM =
    lists:foldl(
      fun(V, M) -> ?SET_STATE(V, 'A', M) end,
      NM, Vs),
  {NQ, NxtM, NP}.

%% BFS loop
-spec bfs_step(graph:graph(), queue:queue(), states(), parents()) -> parents().
bfs_step(Graph, Q, M, P) ->
  case ?IS_EMPTY(Q) of
    'true' ->
      P;
    'false' ->
      {{U, UCost}, NQ} = ?EXTRACT_FROM_QUEUE(Q),
      NM = ?SET_STATE(U, 'E', M),
      Neighbours = graph:out_neighbours(Graph, U),
      {NxtQ, NxtM, NxtP} =
        lists:foldl(
          fun(V, {FQ, FM, FP}) ->
            case ?GET_STATE(V, FM) of
              'A' ->
                W = graph:edge_weight(Graph, {U, V}),
                QQ = ?ADD_TO_QUEUE(V, UCost + W, FQ),
                MM = ?SET_STATE(V, 'Y', FM),
                PP = ?ADD_TO_PARENTS(V, UCost + W, U, FP),
                {QQ, MM, PP};
              _ ->
                {FQ, FM, FP}
            end
          end,
          {NQ, NM, P},
          Neighbours
        ),
      bfs_step(Graph, NxtQ, NxtM, NxtP)
  end.
  
