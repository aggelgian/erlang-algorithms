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
%% @doc Kruskal Algorithm
%%

-module(kruskal).

-export([run/1]).

-type result_set() :: {non_neg_integer(), graph:weight(), [graph:edge()]}.

%% ==========================================================
%% Exported Functions
%% ==========================================================

%% @doc Runs the Kruskal algorithm on a graph <code>Graph</code>.
%% <p>The result is a tuple <code>{Cost, MST}</code> where
%% <code>Cost :: term()</code> is the cost of the
%% <code>MST</code>.</p>
-spec run(graph:graph()) -> graph_lib:mst_info().
run(Graph) ->
  {UF, Es, N, R} = kruskal_init(Graph),
  {_Sz, Cost, MST} = kruskal_step(UF, Es, N, R),
  {Cost, MST}.

%% ==========================================================
%% Kruskal Functions
%% ==========================================================

%% Initialize data structures
-spec kruskal_init(graph:graph()) -> {union_find:uf_forest(), [{graph:edge(), graph:weight()}], non_neg_integer(), result_set()}.
kruskal_init(Graph) ->
  Vs = graph:vertices(Graph),
  N = length(Vs),
  WEs = graph:edges_with_weights(Graph),
  SEs = lists:sort(fun({_E1, W1}, {_E2, W2}) -> W1 < W2 end, WEs),
  UF = union_find:singletons_from_list(Vs),
  {UF, SEs, N, empty_result()}.
  
%% Kruskal loop
-spec kruskal_step(union_find:uf_forest(), [{graph:edge(), graph:weight()}], non_neg_integer(), result_set()) -> result_set().
kruskal_step(UF, [], _N, Res) ->
  union_find:delete(UF),
  Res;
kruskal_step(UF, [{{X, Y}, _W}=E|Es], N, Res) ->
  case result_size(Res) < N-1 of
    'true' ->
      ParX = union_find:find(UF, X),
      ParY = union_find:find(UF, Y),
      case ParX =:= ParY of
        'true' ->
          kruskal_step(UF, Es, N, Res);
        'false' ->
          'true' = union_find:union(UF, ParX, ParY),
          kruskal_step(UF, Es, N, add_to_result(Res, E))
      end;
    'false' ->
      union_find:delete(UF),
      Res
  end.
  
%% ----------------------------------------------------------
%% Kruskal Abstractions
%% ----------------------------------------------------------

%% Result Set Abstrations
%% Result Set :: {Vertices, Cost, MST}
empty_result() -> {0, 0, []}.
add_to_result({Sz, Cost, Es}, {E, W}) -> {Sz+1, Cost+W, [E|Es]}.
result_size({Sz, _Cost, _Es}) -> Sz.

