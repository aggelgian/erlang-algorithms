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
%% Union / Find
%%

%% This module implements the Union / Find data structure.
%% The implementation is based on ETS tables for the O(1) lookup.
%% It is optimized for the basic union / find operations:
%% *  Union/3 in O(1)
%% *  Find/2 in ammortized O(m α(n, m)) where m >= n finds
%%    and n unions and α(n, m) is the reverse Ackermann function (practically O(m))
%% *  singletons_from_list/1, singletons_from_list/2 in O(n)

-module(union_find).

%% External Exports
-export([singletons_from_list/1, singletons_from_list/2, union/3, find/2,
         delete/1, number_of_sets/1, set_size/2, set_elements/2, pprint/1]).

%% Exported Types
-export_type([uf_forest/0]).

%% Macros
-define(UNDEF_ELEMENT, {'error', 'undef_element'}).
-define(UNDEF_SIZE, 'undef').

%% Types Declarations
-type uf_forest() :: ets:tid().
-type uf_find()   :: term() | uf_undef().
-type uf_size()   :: pos_integer() | uf_undef().
-type uf_union()  :: 'true' | uf_undef() | {'error', 'not_parent_elements'}.
-type uf_undef()  :: ?UNDEF_ELEMENT.

%% =======================================================================
%% External Exports
%% =======================================================================

%% Create a forest of singleton sets from a list of terms
-spec singletons_from_list([term(), ...]) -> uf_forest().

singletons_from_list([]) ->
  erlang:error('badarg');
singletons_from_list(L) when is_list(L) ->
  singletons_from_list(fun id/1, L, ets:new(?MODULE, ['ordered_set']));
singletons_from_list(_L) ->
  erlang:error('badarg').
  
%% Create a forest of singleton sets from a list of terms
%% but applies Fun to each term before adding it
-spec singletons_from_list(function(), [term(), ...]) -> uf_forest().

singletons_from_list(_Fun, []) ->
  erlang:error('badarg');
singletons_from_list(Fun, L) when is_list(L), is_function(Fun) ->
  singletons_from_list(Fun, L, ets:new(?MODULE, ['ordered_set']));
singletons_from_list(_Fun, _L) ->
  erlang:error('badarg').
  
%% Helper function singletons_from_list/3
-spec singletons_from_list(function(), [term()], uf_forest()) -> uf_forest().
  
singletons_from_list(_Fun, [], Forest) ->
  Forest;
singletons_from_list(Fun, [I|Is], Forest) ->
  ets:insert(Forest, {Fun(I), {'root', 1}}),
  singletons_from_list(Fun, Is, Forest).
  
%% Union of two sets (need the parent elements for this)
-spec union(uf_forest(), term(), term()) -> uf_union().
  
union(_Forest, _X, _X) ->
  'true';
union(Forest, X, Y) ->
  case {ets:lookup(Forest, X), ets:lookup(Forest, Y)} of
    {[{X, {'root', SzX}}], [{Y, {'root', SzY}}]} ->
      ets:insert(Forest, {Y, {X, ?UNDEF_SIZE}}),
      ets:insert(Forest, {X, {'root', SzX + SzY}});
    {[], _} ->
      ?UNDEF_ELEMENT;
    {_, []} ->
      ?UNDEF_ELEMENT;
    {_, _} ->
      {'error', 'not_parent_elements'}
  end.

%% Find the parent element of the set which a term belongs to
-spec find(uf_forest(), term()) -> uf_find().

find(Forest, X) ->
  find_and_compress(Forest, X, []).
  
-spec find_and_compress(uf_forest(), term(), [term()]) -> uf_find().

find_and_compress(Forest, X, Es) ->
  case ets:lookup(Forest, X) of
    [] ->
      ?UNDEF_ELEMENT;
    [{X, {'root', _SzX}}] ->
      lists:foreach(fun(E) -> ets:insert(Forest, {E, {X, ?UNDEF_SIZE}}) end, Es),
      X;
    [{X, {ParX, ?UNDEF_SIZE}}] ->
      find_and_compress(Forest, ParX, [X|Es])
  end.

%% Delete a forest
-spec delete(uf_forest()) -> 'true'.

delete(Forest) ->
  ets:delete(Forest).
  
%% Return the size of the set which an element belongs to
-spec set_size(uf_forest(), term()) -> uf_size().

set_size(Forest, X) ->
  case ets:lookup(Forest, X) of
    [] -> ?UNDEF_ELEMENT;
    [{X, {'root', Sz}}] -> Sz;
    [{X, {ParX, ?UNDEF_SIZE}}] -> set_size(Forest, ParX)
  end.
  
%% Get all the elements of the set which an element belongs to
-spec set_elements(uf_forest(), term()) -> [term(), ...].
  
set_elements(Forest, X) ->
  Root = find(Forest, X),
  Es = lists:flatten(ets:match(Forest, {'$1', {'_', '_'}})),
  lists:filter(fun(E) -> Root =:= find(Forest, E) end, Es).

%% Return the number of sets that exist in a forest
-spec number_of_sets(uf_forest()) -> non_neg_integer().

number_of_sets(Forest) ->
  length(ets:match_object(Forest, {'_', {'root', '_'}})).
  
%% Pretty print the sets of a forest
-spec pprint(uf_forest()) -> 'ok'.

pprint(Forest) ->
  Es = lists:flatten(ets:match(Forest, {'$1', {'_', '_'}})),
  pprint_sets(Forest, Es).
  
-spec pprint_sets(uf_forest(), [term()]) -> 'ok'.
pprint_sets(_Forest, []) ->
  'ok';
pprint_sets(Forest, [E|_]=All) ->
  Root = find(Forest, E),
  io:format("Set with root : ~w~n", [Root]),
  SetEs = set_elements(Forest, E),
  io:format("Elements: ~w~n", [SetEs]),
  pprint_sets(Forest, All -- SetEs).
  
-spec id(term()) -> term().
id(Arg) -> Arg.

