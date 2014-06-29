-module(graph_demo).
-export([from_file_default/0, from_file_custom/0, manual/0, g1/0, g2/0, g3/0, g4/0]).

%% Load a graph from file using the default file syntax.
-spec from_file_default() -> ok.

from_file_default() ->
  G = g1(),
  graph:pprint(G).

-spec g1() -> graph:graph().
g1() ->
  File = ?DEMO_DATA ++ "/graph1.txt",
  graph:from_file(File).

%% Load a graph from file using custom vertices.
-spec from_file_custom() -> ok.

from_file_custom() ->
  G = g4(),
  graph:pprint(G).

-spec g4() -> graph:graph().
g4() ->
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
  graph:from_file(File, ReadVertices, ReadEdge).

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


-spec g2() -> graph:graph().
g2() ->
  File = ?DEMO_DATA ++ "/graph2.txt",
  graph:from_file(File).

-spec g3() -> graph:graph().
g3() ->
  File = ?DEMO_DATA ++ "/graph3.txt",
  graph:from_file(File).
