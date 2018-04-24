%% @doc Edmonds-Karp / Ford-Fulkerson Algorithms
%%
%% <p>Calculates the Maximum Flow in a Network (Directed Graph)</p>
%% 
%% <p>For examples you can check the <code>flow_demo</code> module.</p>
%% 

-module(edmonds_karp).

-export([run/4]).

-type mode() :: bfs | dfs.

%% ==========================================================
%% Exported Functions
%% ==========================================================

%% @doc Runs the Edmonds-Karp or Ford-Fulkerson algorithm 
%% on a graph <code>G</code> with <code>S</code> as source 
%% as <code>T</code> as sink.
%%
%% <p>When <code>Mode</code> is <code>dfs</code> the algorithm is 
%% called Ford-Fulkerson and when <code>Mode</code> is 
%% <code>bfs</code> the algorithm is called Edmonds-Karp.</p>
%%
-spec run(graph:graph(), graph:vertex(), graph:vertex(), mode()) -> graph_lib:flow() | {'error', 'not_network'}.

run(G, S, T, Mode) when Mode =:= bfs; Mode =:= dfs ->
  case graph:graph_type(G) of
    directed ->
      {Flow, RN} = init_residual_network(G),
      ok = edmonds_karp_step(G, RN, Flow, S, T, Mode),
      Out = graph_lib:reconstruct_flow(ets:tab2list(Flow)),
      %% clean up residual graph and flow list
      graph:del_graph(RN),
      ets:delete(Flow),
      Out;
    undirected ->
      {error, not_network}
  end.

%% ==========================================================
%% Edmonds-Karp / Ford-FulkersonFunctions
%% ==========================================================

%% Edmonds-Karp loop
-spec edmonds_karp_step(graph:graph(), graph:graph(), ets:tid(), graph:vertex(), graph:vertex(), mode()) -> 'ok'.
edmonds_karp_step(G, RN, Flow, S, T, M) ->
  case augmenting_path(RN, S, T, M) of
    no_path -> ok;
    EPath ->
      RN_n = update_residual_network(G, RN, Flow, EPath),
      edmonds_karp_step(G, RN_n, Flow, S, T, M)
  end.

%% Initialize Residual Network
-spec init_residual_network(graph:graph()) -> {ets:tid(), graph:graph()}.
init_residual_network(G) ->
  Vs = graph:vertices(G),
  Es = graph:edges_with_weights(G),
  RN = graph:empty(directed),
  F = ets:new(f, [ordered_set]),
  ets:insert(F, {flow, 0}),
  lists:foreach(fun({E, _W}) -> ets:insert(F, {E, 0}) end, Es),
  lists:foreach(fun(V) -> graph:add_vertex(RN, V) end, Vs),
  lists:foreach(fun({{U, V}, W}) -> graph:add_edge(RN, U, V, W) end, Es),
  {F, RN}.
      
%% Find an augmenting path
-spec augmenting_path(graph:graph(), graph:vertex(), graph:vertex(), mode()) -> graph_lib:epath_weighted() | 'no_path'.
augmenting_path(RN, S, T, M) ->
  Traversal =
    case M of
      dfs -> dfs:run(RN, S);
      bfs -> bfs:run(RN, S)
    end,
  case proplists:get_value(T, Traversal) of
    unreachable  -> no_path;
    {_Cost, VPath} -> path_edges(RN, VPath)
  end.

%% Return the edges visited in a path
-spec path_edges(graph:graph(), graph_lib:vpath()) -> graph_lib:epath_weighted().
path_edges(RN, Path) -> path_edges(RN, Path, []).

%% Helper funciton path_edges/3
-spec path_edges(graph:graph(), graph_lib:vpath(), graph_lib:epath_weighted()) -> graph_lib:epath_weighted().
path_edges(_RN, [_V], Es) ->
  lists:reverse(Es);
path_edges(RN, [U, V|Ps], Es) ->
  E = {U, V},
  W = graph:edge_weight(RN, E),
  path_edges(RN, [V|Ps], [{E, W}|Es]).

%% Update the Residual Network with the information
%% of an augmenting path
-spec update_residual_network(graph:graph(), graph:graph(), ets:tid(), graph_lib:epath_weighted()) -> graph:graph().
update_residual_network(G, RN, Flow, EPath) ->
  %% Get the flow increase
  [{_, Cf}|_] = lists:sort(fun({_E1, W1}, {_E2, W2}) -> W1 < W2 end, EPath),
  %% Update the flow
  [{flow, CurrFlow}] = ets:lookup(Flow, flow),
  ets:insert(Flow, {flow, CurrFlow + Cf}),
  %% Update the residual network
  lists:foreach(
    fun({{U, V}, _W}) ->
      {{From, To}=E, W} =
        case {graph:edge_weight(G, {U,V}), graph:edge_weight(G, {V,U})} of
          {false, C} -> {{V, U}, C};
          {C, false} -> {{U, V}, C}
        end,
      graph:del_edge(RN, {U, V}),
      graph:del_edge(RN, {V, U}),
      [{E, F}] = ets:lookup(Flow, E),
      NF = F + Cf,
      ets:insert(Flow, {E, NF}),
      _ = case W-NF > 0 of
        true  -> graph:add_edge(RN, From, To, W-NF);
        false -> ok
      end,
      _ = case NF > 0 of
        true  -> graph:add_edge(RN, To, From, NF);
        false -> ok
      end
    end,
    EPath
  ),
  RN.

