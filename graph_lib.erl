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
-export([reconstruct_path/2, reconstruct_path/4]).

-export_type([path/0, paths/0]).

-type path() :: [graph:vertex()].
-type paths() :: [{graph:vertex(), {term(), path()}}].

%% ==========================================================
%% Exported Functions
%% ==========================================================

%%% Result :: dict of {Node, {Cost, Prev}}
-spec reconstruct_path(dict(), graph:vertex()) -> paths().
reconstruct_path(Result, Node) ->
  try dict:fetch(Node, Result) of
    {Cost, Prev} ->
      reconstruct_path(Result, Prev, Cost, [Node])
  catch
    error:badarg ->
      unreachable
  end.

reconstruct_path(_Result, root, Cost, Path) ->
  {Cost, Path};
reconstruct_path(Result, Node, Cost, Path) ->
  {_, Prev} = dict:fetch(Node, Result),
  reconstruct_path(Result, Prev, Cost, [Node|Path]).
