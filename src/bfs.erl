%% @doc BFS Algorithm
%%
%% <p>For examples you can check the <code>bfs_demo</code> module.</p>
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
  {Q, M, P, Vertices} = bfs_init(Graph, Root),
  Result = bfs_step(Graph, Q, M, P),
  graph_lib:reconstruct_all_paths(Vertices, Result).

%% ==========================================================
%% BFS Functions
%% ==========================================================

%% Initialize data structures
-spec bfs_init(graph:graph(), graph:vertex()) -> {queue:queue(), states(), parents(), [graph:vertex()]}.
bfs_init(Graph, Root) ->
  Q = ?ADD_TO_QUEUE(Root, 0, ?EMPTY_QUEUE()),
  Ps = ?ADD_TO_PARENTS(Root, 0, root, ?EMPTY_PARENTS()),
  Vs = graph:vertices(Graph),
  Ms = lists:foldl(fun(V, M) -> ?SET_STATE(V, 'A', M) end, ?EMPTY_STATES(), Vs),
  NMs = ?SET_STATE(Root, 'Y', Ms),
  {Q, NMs, Ps, Vs}.

%% BFS loop
-spec bfs_step(graph:graph(), queue:queue(), states(), parents()) -> parents().
bfs_step(Graph, Qe, Ms, Ps) ->
  case ?IS_EMPTY(Qe) of
    true -> Ps;
    false ->
      {{U, UCost}, NQ} = ?EXTRACT_FROM_QUEUE(Qe),
      NMs = ?SET_STATE(U, 'E', Ms),
      Neighbours = graph:out_neighbours(Graph, U),
      {NxtQ, NxtM, NxtP} =
        lists:foldl(
          fun(V, {Q, M, P}) ->
            case ?GET_STATE(V, M) of
              'A' ->
                W = graph:edge_weight(Graph, {U, V}),
                Cost = UCost + W,
                QQ = ?ADD_TO_QUEUE(V, Cost, Q),
                MM = ?SET_STATE(V, 'Y', M),
                PP = ?ADD_TO_PARENTS(V, Cost, U, P),
                {QQ, MM, PP};
              _ -> {Q, M, P}
            end
          end,
          {NQ, NMs, Ps},
          Neighbours
        ),
      bfs_step(Graph, NxtQ, NxtM, NxtP)
  end.
  
