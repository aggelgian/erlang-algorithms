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
%% Library Functions for Graph Algorithms
%%

-module(graph_lib).
-export([reconstruct_all_paths/2]).

-export_type([path/0, path_info/0]).

-type path() :: [graph:vertex()].
-type path_info() :: {graph:vertex(), {term(), path()} | 'unreachable'}.

%% ==========================================================
%% Exported Functions
%% ==========================================================

%%% Result :: dict of {Node, {Cost, Prev}}
-spec reconstruct_path(dict(), graph:vertex()) -> path_info().
reconstruct_path(Result, Node) ->
  try dict:fetch(Node, Result) of
    {Cost, Prev} ->
      {Node, reconstruct_path(Result, Prev, Cost, [Node])}
  catch
    error:badarg ->
      {Node, 'unreachable'}
  end.

-spec reconstruct_path(dict(), graph:vertex(), term(), path()) -> {term(), path()}.
reconstruct_path(_Result, root, Cost, Path) ->
  {Cost, Path};
reconstruct_path(Result, Node, Cost, Path) ->
  {_, Prev} = dict:fetch(Node, Result),
  reconstruct_path(Result, Prev, Cost, [Node|Path]).
  
-spec reconstruct_all_paths([graph:vertex()], dict()) -> [path_info()].
reconstruct_all_paths(Vertices, Result) ->
  SortedVs = lists:sort(fun erlang:'<'/2, Vertices),
  lists:map(fun(V) -> reconstruct_path(Result, V) end, SortedVs).
