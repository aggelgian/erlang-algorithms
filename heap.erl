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
%% Min-Heap, Max-Heap, Priority Queue
%%

%% This module implements min-heaps and max-heaps for use in priority queues.
%% Each value in the heap is assosiated with a reference so that the user can change its priority in O(log n).
%%
%% The implementation is based on ETS tables for the O(1) lookup time.
%% It supports all the basic heap operations:
%% *  min(), max() in O(1)
%% *  delete_head in O(log n)
%% *  insert in O(log n)
%% *  change in O(log n)
%% *  from_list in Θ(n)


-module(heap).
-export([new/1, get_length/1, is_empty/1, max/1, min/1, insert/2, 
  delete_head/1, change/3, from_list/2, delete/1]).

-export_type([mode/0, heap/0]).

-opaque mode() :: max | min.
-opaque heap() :: {mode(), ets:tid()}.


%% =======================================================================
%% External Exports
%% =======================================================================

%% -----------------------------------------------------------------------
%% new(Order) -> Heap
%%   Order :: mode()
%%   Heap  :: heap()
%% Creates an empty heap
%% -----------------------------------------------------------------------
new(Order) when Order =:= max; Order=:= min ->
  H = ets:new(?MODULE, [ordered_set, public]),
  ets:insert(H, {length, 0}),
  {Order, H}.
  
%% -----------------------------------------------------------------------
%% delete(Heap) -> true
%%   Heap :: heap()
%% Deletes the heap
%% -----------------------------------------------------------------------
delete({_Order, H}) ->
  ets:delete(H).

%% -----------------------------------------------------------------------
%% get_length(Heap) -> Length
%%   Heap   :: heap()
%%   Length :: non_neg_integer()
%% Returns the number of elements that the Heap contains
%% -----------------------------------------------------------------------
get_length({_Order, H}) ->
  [{length, L}] = ets:lookup(H, length),
  L.
  
%% -----------------------------------------------------------------------
%% is_empty(Heap) -> Empty
%%   Heap  :: heap()
%%   Empty :: boolean()
%% Checks whether Heap is an empty heap or not
%% -----------------------------------------------------------------------
is_empty(H) ->
  get_length(H) =:= 0.
  
%% -----------------------------------------------------------------------
%% max(Heap) -> Max
%%   Heap :: heap()
%%   Max  :: term()
%% Returns the element of the heap with the maximum priority.
%% If Heap is a minimum priority heap, it returns {error, min_heap}
%% -----------------------------------------------------------------------
max({max, H}) ->
  case ets:lookup(H, 1) of
    [] ->
      {error, empty_heap};
    [{1, {Max, _R}}] ->
      Max
  end;
max({min, _H}) ->
  {error, min_heap}.
  
%% -----------------------------------------------------------------------
%% min(Heap) -> Min
%%   Heap :: heap()
%%   Min  :: term()
%% Returns the element of the heap with the minimum priority.
%% If Heap is a maximum priority heap, it returns {error, max_heap}
%% -----------------------------------------------------------------------
min({min, H}) ->
  case ets:lookup(H, 1) of
    [] ->
      {error, empty_heap};
    [{1, {Min, _R}}] ->
      Min
  end;
min({max, _H}) ->
  {error, max_heap}.
  
  
%% -----------------------------------------------------------------------
%% insert(Heap, Element) -> {Element, Ref}
%%   Heap    :: heap()
%%   Element :: term()
%%   Ref     :: reference()
%% Add a new element to the Heap and returns a tuple with the element
%% and a reference so that one can change its priority
%% -----------------------------------------------------------------------
insert({_Order, H}=HO, X) ->
  HS = get_length(HO),
  NHS = HS + 1,
  R = erlang:make_ref(),
  ets:insert(H, {NHS, {X, R}}),
  ets:insert(H, {R, NHS}),
  ets:insert(H, {length, NHS}),
  I = NHS,
  P = I div 2,
  insert_loop(HO, I, P),
  {X, R}.
  
%% -----------------------------------------------------------------------
%% delete_head(Heap) -> Element
%%   Heap    :: heap()
%%   Element :: term()
%% Deletes and returns the element at the top of the heap
%% and re-arranges the rest of the heap
%% -----------------------------------------------------------------------
delete_head({_Order, H}=HO)->
  case ets:lookup(H, 1) of
    [] ->
      {error, empty_heap};
    [{1, {Head, RHead}}] ->
      HS = get_length(HO),
      [{HS, {X, RX}}] = ets:lookup(H, HS),
      ets:delete(H, HS),                %% Can be commented
      ets:delete(H, RHead),             %% Can be commented
      NHS = HS - 1,
      ets:insert(H, {length, NHS}),
      case NHS =:= 0 of                 %% Can be commented
        true  -> ok;                    %% Can be commented
        false ->                        %% Can be commented
          ets:insert(H, {1, {X, RX}}),
          ets:insert(H, {RX, 1})
      end,                              %% Can be commented
      combine(HO, 1, NHS),
      Head
  end.

%% -----------------------------------------------------------------------
%% change(Heap, Ref, Value) -> true
%%   Heap  :: heap()
%%   Ref   :: reference()
%%   Value :: term()
%% Changes the priority of the element referenced with Ref to Value
%% and then re-arranges the heap
%% -----------------------------------------------------------------------
change({Order, H}=HO, R, X) ->
  [{R, I}] = ets:lookup(H, R),
  [{I, {OldX, R}}] = ets:lookup(H, I),
  case {X > OldX, Order} of
    {true, max} ->
      to_top(HO, I, X, R);
    {false, min} ->
      to_top(HO, I, X, R);
    {false, max}->
      to_bottom(HO, I, X, R);
    {true, min} ->
      to_bottom(HO, I, X, R)
  end,
  true.

%% -----------------------------------------------------------------------
%% from_list(Order, List) -> {H, RefList}
%%   Order   :: mode()
%%   List    :: [{non_neg_integer(), term()}]
%%   H       :: heap()
%%   RefList :: [{non_neg_integer(), referenc()}]
%% -----------------------------------------------------------------------
from_list(Order, L) when is_list(L) ->
  HS = length(L),
  {HO, Rs} = ets_from_elements(Order, L, HS),
  I = HS div 2,
  construct_heap(HO, I, HS),
  {HO, Rs}.
  
%% =======================================================================
%% Internal Functions
%% =======================================================================

%% Re-arranges the heap in a bottom-up manner
insert_loop({Order, H}=HO, I, P) when I > 1 ->
  [{I, {X, _RX}}] = ets:lookup(H, I),
  [{P, {Y, _RY}}] = ets:lookup(H, P),
  case {Y < X, Order} of
    {true, max} ->
      swap(H, P, I),
      NI = P,
      NP = NI div 2,
      insert_loop(HO, NI, NP);
    {false, min} ->
      swap(H, P, I),
      NI = P,
      NP = NI div 2,
      insert_loop(HO, NI, NP);
    {_, _} ->
      ok
  end;
insert_loop(_HO, _I, _P) -> ok.

  
to_top({_Order, H}=HO, I, X, R) ->
  ets:insert(H, {I, {X, R}}),
  P = I div 2,
  insert_loop(HO, I, P).

%% Re-arranges the heap in a top-down manner
combine(HO, I, HS) ->
  L = 2*I,
  R = 2*I + 1,
  MP = I,
  MP_L = combine_h1(HO, L, MP, HS),
  MP_R = combine_h1(HO, R, MP_L, HS),
  combine_h2(HO, MP_R, I, HS).
  
combine_h1({Order, H}, W, MP, HS) when W =< HS ->
  [{W, {X, _RX}}] = ets:lookup(H, W),
  [{MP, {Y,_RY}}] = ets:lookup(H, MP),
  case {X > Y, Order} of
    {true, max}  -> W;
    {false, min} -> W;
    {_, _} -> MP
  end;
combine_h1(_HO, _W, MP, _HS) -> MP.
  
combine_h2(_HO, MP, I, _HS) when MP =:= I ->
  ok;
combine_h2({_Order, H}=HO, MP, I, HS) ->
  swap(H, I, MP),
  combine(HO, MP, HS).
  
to_bottom({_Order, H}=HO, I, X, R) ->
  ets:insert(H, {I, {X, R}}),
  HS = get_length(HO),
  combine(HO, I, HS).
  
%% Swaps two elements of the heap
swap(H, I, J) ->
  [{I, {X, RX}}] = ets:lookup(H, I),
  [{J, {Y, RY}}] = ets:lookup(H, J),
  ets:insert(H, {I, {Y, RY}}),
  ets:insert(H, {RY, I}),
  ets:insert(H, {J, {X, RX}}),
  ets:insert(H, {RX, J}).
  
%% Used for constructing a heap from a list
construct_heap(HO, I, HS) when I > 0 ->
  combine(HO, I, HS),
  construct_heap(HO, I-1, HS);
construct_heap(_HO, _I, _HS) -> ok.
  
ets_from_elements(Order, L, HS) ->
  HO = new(Order),
  {Order, H} = HO,
  ets:insert(H, {length, HS}),
  Rs = add_elements(HO, L, []),
  {HO, Rs}.  
  
add_elements(_HO, [], Acc) ->
  lists:reverse(Acc);
add_elements({_Order, H}=HO, [{Key, Val}|Es], Acc) ->
  R = erlang:make_ref(),
  ets:insert(H, {Key, {Val, R}}),
  ets:insert(H, {R, Key}),
  add_elements(HO, Es, [{Key, R}|Acc]).
  

