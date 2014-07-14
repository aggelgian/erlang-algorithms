-module(a_star_demo).
-export([b1/0]).

parse_vertex([$(, X, $,,  Y, $)]) -> {X - $0, Y - $0}.

-spec b1() -> ok.
b1() ->
  File = ?DEMO_DATA ++ "/board1.txt",
  %% Function to read the vertices
  ReadVertices =
    fun(IO, _N) -> 
      Ln = io:get_line(IO, ">"),
      Ts = string:tokens(string:strip(Ln, right, $\n), " "),
      [parse_vertex(T) || T <- Ts]
    end,
  %% Function to read each edge
  ReadEdge =
    fun(IO, _WT) ->
      {ok, [V1, V2, W]} = io:fread(IO, ">", "~s ~s ~d"),
      {parse_vertex(V1), parse_vertex(V2), W}
    end,
  %% Heurestic underestimate function
  F = fun({X1, Y1}, {X2, Y2}) -> abs(X1 - X2) + abs(Y1 - Y2) end,
  G = graph:from_file(File, ReadVertices, ReadEdge),
  {Cost, Path} = a_star:run(G, {1,1}, {6,6}, F),
  io:format("Cost: ~p~nPath: ~p~n", [Cost, Path]).
