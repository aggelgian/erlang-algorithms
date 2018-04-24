%% @doc Union / Find
%%
%% <p>This module implements the Union / Find data structure.</p>
%%
%% <p>
%%   The implementation is based on ETS tables for the O(1) lookup.
%%   It is optimized for the basic union / find operations:
%%   <ul>
%%     <li>
%%       <code>union/3</code> in <em>O(1)</em>
%%     </li>
%%     <li>
%%       <code>find/2</code> in ammortized <em>O(m a(n, m))</em> where
%%       <em>m >= n</em> finds and <em>n</em> unions and <em>a(n, m)</em> is the
%%       reverse Ackermann function. Practically, the overall 
%%       complexity is <em>O(m)</em>.
%%     </li>
%%     <li>
%%       <code>singletons_from_list/1</code>, <code>singletons_from_list/2</code>
%%       in <em>O(n)</em>.
%%     </li>
%%   </ul>
%% </p>
%%
%% <p>For examples you can check the <code>union_find_demo</code> module.</p>
%% 

-module(union_find).

-export([singletons_from_list/1, singletons_from_list/2, union/3, find/2,
         delete/1, number_of_sets/1, set_size/2, set_elements/2, pprint/1]).

-export_type([uf_forest/0]).

-define(UNDEF_SIZE, undef).

%%
%% @type uf_forest(). A forest of union-find sets.
%%
-type uf_forest() :: ets:tid().
-type uf_find()   :: term() | uf_undef().
-type uf_size()   :: pos_integer() | uf_undef().
-type uf_union()  :: true | uf_undef() | {error, not_parent_elements}.
-type uf_undef()  :: {error, undef_element}.

%% =======================================================================
%% External Exports
%% =======================================================================

%% @doc Create a forest of singleton sets from a list of terms.
-spec singletons_from_list([term(), ...]) -> uf_forest().

singletons_from_list([]) ->
  erlang:error(badarg);
singletons_from_list(L) when is_list(L) ->
  singletons_from_list(fun id/1, L, ets:new(?MODULE, [ordered_set]));
singletons_from_list(_L) ->
  erlang:error(badarg).

%% @doc Create a forest of singleton sets from a list of terms.
%% <p>Same as <code>singletons_from_list/1</code> but it applies
%% <code>Fun</code> to each term before adding it to the forest.</p>
-spec singletons_from_list(function(), [term(), ...]) -> uf_forest().

singletons_from_list(_Fun, []) ->
  erlang:error(badarg);
singletons_from_list(Fun, L) when is_list(L), is_function(Fun) ->
  singletons_from_list(Fun, L, ets:new(?MODULE, [ordered_set]));
singletons_from_list(_Fun, _L) ->
  erlang:error(badarg).

%% Helper function singletons_from_list/3
-spec singletons_from_list(function(), [term()], uf_forest()) -> uf_forest().

singletons_from_list(_Fun, [], Forest) ->
  Forest;
singletons_from_list(Fun, [I|Is], Forest) ->
  ets:insert(Forest, {Fun(I), {root, 1}}),
  singletons_from_list(Fun, Is, Forest).

%% @doc Union of two sets.
%% <p>The parent elements of the two sets are needed.</p>
-spec union(uf_forest(), term(), term()) -> uf_union().

union(_Forest, _X, _X) -> true;
union(Forest, X, Y) ->
  case {ets:lookup(Forest, X), ets:lookup(Forest, Y)} of
    {[{X, {root, SzX}}], [{Y, {root, SzY}}]} ->
      ets:insert(Forest, {Y, {X, ?UNDEF_SIZE}}),
      ets:insert(Forest, {X, {root, SzX + SzY}});
    {[], _} ->
      {error, undef_element};
    {_, []} ->
      {error, undef_element};
    {_, _} ->
      {error, not_parent_elements}
  end.

%% @doc Find the parent element of the set which a term belongs to.
-spec find(uf_forest(), term()) -> uf_find().

find(Forest, X) -> find_and_compress(Forest, X, []).

-spec find_and_compress(uf_forest(), term(), [term()]) -> uf_find().

find_and_compress(Forest, X, Es) ->
  case ets:lookup(Forest, X) of
    [] ->
      {error, undef_element};
    [{X, {root, _SzX}}] ->
      lists:foreach(fun(E) -> ets:insert(Forest, {E, {X, ?UNDEF_SIZE}}) end, Es),
      X;
    [{X, {ParX, ?UNDEF_SIZE}}] ->
      find_and_compress(Forest, ParX, [X|Es])
  end.

%% @doc Delete a forest
-spec delete(uf_forest()) -> true.

delete(Forest) -> ets:delete(Forest).

%% @doc Return the size of the set which an element belongs to
-spec set_size(uf_forest(), term()) -> uf_size().

set_size(Forest, X) ->
  case ets:lookup(Forest, X) of
    [] -> {error, undef_element};
    [{X, {root, Sz}}] -> Sz;
    [{X, {ParX, ?UNDEF_SIZE}}] -> set_size(Forest, ParX)
  end.

%% @doc Return a list of all the elements of the set
%% which an element belongs to.
-spec set_elements(uf_forest(), term()) -> [term(), ...].

set_elements(Forest, X) ->
  Root = find(Forest, X),
  Es = lists:flatten(ets:match(Forest, {'$1', {'_', '_'}})),
  lists:filter(fun(E) -> Root =:= find(Forest, E) end, Es).

%% @doc Return the number of sets that exist in a forest
-spec number_of_sets(uf_forest()) -> non_neg_integer().

number_of_sets(Forest) -> length(ets:match_object(Forest, {'_', {root, '_'}})).

%% @doc Pretty print the sets of a forest
-spec pprint(uf_forest()) -> ok.

pprint(Forest) ->
  Es = lists:flatten(ets:match(Forest, {'$1', {'_', '_'}})),
  pprint_sets(Forest, Es).

-spec pprint_sets(uf_forest(), [term()]) -> ok.
pprint_sets(_Forest, []) -> ok;
pprint_sets(Forest, [E|_]=All) ->
  Root = find(Forest, E),
  io:format("Set with root : ~w~n", [Root]),
  SetEs = set_elements(Forest, E),
  io:format("Elements: ~w~n", [SetEs]),
  pprint_sets(Forest, All -- SetEs).
  
-spec id(term()) -> term().
id(Arg) -> Arg.

