-module(graph_demo).
-export([from_file_default/0, from_file_custom/0, manual/0]).

%% Load a graph from file using the default file syntax.
-spec from_file_default() -> ok.

from_file_default() ->
  File = ?DEMO_DATA ++ "/graph1.txt",
  G = graph:from_file(File),
  graph:pprint(G).

%% Load a graph from file using custom vertices.
-spec from_file_custom() -> ok.

from_file_custom() ->
  File = ?DEMO_DATA ++ "/graph4.txt",
  %% Function to read the vertices
  ReadVertices =
    fun(IO, _N) -> 
      Ln = io:get_line(IO, ">"),
      string:tokens(string:strip(Ln, right, $\n), " ")
    end,
  %% Function to read each edge
  ReadEdge =
    fun(IO, _WT) ->
      {ok, [V1, V2, W]} = io:fread(IO, ">", "~s ~s ~d"),
      {V1, V2, W}
    end,
  G = graph:from_file(File, ReadVertices, ReadEdge),
  graph:pprint(G).

%% Manually create a graph.
-spec manual() -> ok.

manual() ->
  G = graph:empty(undirected), %% Empty graph
  %% Add vertices
  lists:foreach(fun(V) -> graph:add_vertex(G, V) end, [athens, paris, london]),
  %% Add edges
  Es = [{athens, paris, 2096}, {athens, london, 2389}, {paris, london, 342}],
  lists:foreach(fun({F, T, W}) -> graph:add_edge(G, F, T, W) end, Es),
  graph:pprint(G).
