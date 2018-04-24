%% @doc A* Algorithm
%% 
%% <p>A* calculates the least expensive path from a <code>Root</code> node to a <code>Target</code>
%% node. It uses an admissible heuristic function <code>H</code> (that underestimates the cost) in order to visit
%% nodes that are more likely to lead to the <code>Target</code> faster.</p>
%% 
%% <p>For examples you can check the <code>a_star_demo</code> module.</p>
%% 

-module(a_star).
-export([run/4]).

-export_type([astar_result/0]).

-type open_set()   :: dict:dict().
-type closed_set() :: gb_sets:set().
-type f_score()    :: heap:heap().
-type g_score()    :: dict:dict().
-type parents()    :: dict:dict().

-record(sts, {
  graph   :: graph:graph(),
  target  :: graph:vertex(),
  h       :: function(),
  open    :: open_set(),
  closed  :: closed_set(),
  fscore  :: f_score(),
  gscore  :: g_score(),
  parents :: parents()
}).
-type astar_state()  :: #sts{}.
-type astar_result() :: {graph:weight(), graph_lib:vpath()} | unreachable.

%% ----------------------------------------------------------
%% A* Abstractions
%% ----------------------------------------------------------

%% Graph Abstractions
-define(GET_NEIGHBOURS(Vertex, Graph), graph:out_neighbours(Graph, Vertex)).
-define(EDGE_WEIGHT(Graph, From, To), graph:edge_weight(Graph, {From, To})).
%% Open Set Abstractions
-define(EMPTY_OPEN, dict:new()).
-define(IS_OPEN_EMPTY(Open), dict:size(Open) =:= 0).
-define(ADD_TO_OPEN(Node, HeapRef, Open), dict:store(Node, HeapRef, Open)).
-define(REMOVE_FROM_OPEN(Node, Open), dict:erase(Node, Open)).
-define(IS_IN_OPEN(Node, Open), dict:is_key(Node, Open)).
-define(GET_REF(Node, Open), dict:fetch(Node, Open)).
%% Closed Set Abstractions
-define(EMPTY_CLOSED, gb_sets:new()).
-define(ADD_TO_CLOSED(Node, Closed), gb_sets:add(Node, Closed)).
-define(IS_IN_CLOSED(Node, Closed), gb_sets:is_element(Node, Closed)).
%% F score Abstractions
-define(EMPTY_FSCORE, heap:new(min)).
-define(SET_FSCORE(Node, Score, Fscore), erlang:element(2,heap:insert(Fscore, {Score, Node}))).
-define(MIN_FSCORE(Fscore), erlang:element(2, heap:take_min(Fscore))).
-define(UPDATE_FSCORE(Node, Ref, Score, Fscore), heap:update(Fscore, Ref, {Score, Node})).
%% G score Abstractions
-define(EMPTY_GSCORE, dict:new()).
-define(SET_GSCORE(Node, Score, Gscore), dict:store(Node, Score, Gscore)).
-define(UPDATE_GSCORE(Node, Score, Gscore), dict:store(Node, Score, Gscore)).
-define(GET_GSCORE(Node, Gscore), dict:fetch(Node, Gscore)).
%% Parents Abstractions
-define(EMPTY_PARENTS, dict:new()).
-define(ADD_PARENT(Node, Prev, Ps), dict:store(Node, Prev, Ps)).
-define(SET_PARENT(Node, Prev, Ps), dict:store(Node, Prev, Ps)).
-define(GET_PARENT(Node, Ps), dict:fetch(Node, Ps)).

%% ==========================================================
%% Exported Functions
%% ==========================================================

%% @doc Runs the A* algorithm on a graph <code>Graph</code>
%% with <code>Root</code> as the point of origin 
%% and <code>Target</code> as the point of destination.
%% 
%% <p>If such a path exists, it returns a tuple with the cost 
%% and the actual best path. If not, it returns <code>unreachable</code>.</p>
%%
%% <p><code>H</code> is the admissible heuristic function. Its spec is 
%% <code>fun(Node :: graph:vertex(), Target :: graph:vertex()) -> EstimatedCost :: graph:weight().</code></p>
-spec run(graph:graph(), graph:vertex(), graph:vertex(), function()) -> astar_result().

run(Graph, Root, Target, H) ->
  State = astar_init(Graph, Root, Target, H),
  R = astar_step(State),
  astar_cleanup(State),
  R.

%% ==========================================================
%% A* Functions
%% ==========================================================

%% Initalize State Graph, Target Node, H function, Open Set, Closed Set, Fscore, Gscore, Parents
-spec astar_init(graph:graph(), graph:vertex(), graph:vertex(), function()) -> astar_state().
astar_init(G, Root, Target, H) ->
  Fscore = ?EMPTY_FSCORE,
  RootRef = ?SET_FSCORE(Root, H(Root, Target), Fscore),
  Open = ?ADD_TO_OPEN(Root, RootRef, ?EMPTY_OPEN),
  Csd = ?EMPTY_CLOSED,
  Gscore = ?SET_GSCORE(Root, 0, ?EMPTY_GSCORE),
  Ps = ?ADD_PARENT(Root, root, ?EMPTY_PARENTS),
  #sts{graph=G, target=Target, h=H, open=Open, closed=Csd, fscore=Fscore, gscore=Gscore, parents=Ps}.

%% Cleanup the state.
astar_cleanup(State) ->
  heap:delete(State#sts.fscore).

%% A* loop
-spec astar_step(astar_state()) -> astar_result().
astar_step(S) ->
  case ?IS_OPEN_EMPTY(S#sts.open) of 
    true -> unreachable;
    false -> astar_step_reachable(S)
  end.

%% A* loop (helper function)
-spec astar_step_reachable(astar_state()) -> astar_result().
astar_step_reachable(S) ->
  Node = ?MIN_FSCORE(S#sts.fscore),
  case Node =:= S#sts.target of
    true -> astar_result(Node, S);
    false ->
      Open_n = ?REMOVE_FROM_OPEN(Node, S#sts.open),
      Csd_n = ?ADD_TO_CLOSED(Node, S#sts.closed),
      Moves = ?GET_NEIGHBOURS(Node, S#sts.graph),
      Gcurr = ?GET_GSCORE(Node, S#sts.gscore),
      S_n = consider(Moves, Node, Gcurr, S#sts{open=Open_n, closed=Csd_n}),
      astar_step(S_n)
  end.

%% Consider all the possible moves and update the state (if necessary)
-spec consider([graph:vertex()], graph:vertex(), graph:weight(), astar_state()) -> astar_state().
consider([], _Curr, _Gcurr, S) -> S;
consider([M|Ms], Curr, Gcurr, S) ->
  case ?IS_IN_CLOSED(M, S#sts.closed) of
    true -> consider(Ms, Curr, Gcurr, S);
    false ->
      Gv = Gcurr + ?EDGE_WEIGHT(S#sts.graph, Curr, M),
      case ?IS_IN_OPEN(M, S#sts.open) of
        true ->
          G_old = ?GET_GSCORE(M, S#sts.gscore),
          case Gv =< G_old of 
            false -> consider(Ms, Curr, Gcurr, S);
            true ->
              Gscore_n = ?UPDATE_GSCORE(M, Gv, S#sts.gscore),
              Ref = ?GET_REF(M, S#sts.open),
              Fv = Gv + (S#sts.h)(M, S#sts.target),
              true = ?UPDATE_FSCORE(M, Ref, Fv, S#sts.fscore),
              Ps_n = ?SET_PARENT(M, Curr, S#sts.parents),
              consider(Ms, Curr, Gcurr, S#sts{gscore=Gscore_n, parents=Ps_n})
          end;
        false ->
          Fv = Gv + (S#sts.h)(M, S#sts.target),
          MRef = ?SET_FSCORE(M, Fv, S#sts.fscore),
          Open_n = ?ADD_TO_OPEN(M, MRef, S#sts.open),
          Gscore_n = ?SET_GSCORE(M, Gv, S#sts.gscore),
          Ps_n = ?ADD_PARENT(M, Curr, S#sts.parents),
          consider(Ms, Curr, Gcurr, S#sts{open=Open_n, gscore=Gscore_n, parents=Ps_n})
      end
  end.

%% Form the result of the algorithm
-spec astar_result(graph:vertex(), astar_state()) -> astar_result().
astar_result(Node, S) ->
  Gv = ?GET_GSCORE(Node, S#sts.gscore),
  Path = astar_path(Node, S#sts.parents),
  {Gv, Path}.

%% Reconstruct the path
-spec astar_path(graph:vertex(), parents()) -> graph_lib:vpath().
astar_path(Node, Ps) -> astar_path(Node, Ps, [Node]).

%% Reconstruct the path (helper function)
-spec astar_path(graph:vertex(), parents(), graph_lib:vpath()) -> graph_lib:vpath().
astar_path(Node, Ps, Acc) ->
  case ?GET_PARENT(Node, Ps) of
    root -> Acc;
    P -> astar_path(P, Ps, [P|Acc])
  end.
