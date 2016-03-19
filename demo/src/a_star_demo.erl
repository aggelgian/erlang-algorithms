-module(a_star_demo).
-export([b1/0, dump_vertex/1]).

%% The representation of a vertex.
-type my_vertex() :: {integer(), integer()}.

-spec b1() -> ok.
b1() ->
  File = ?DEMO_DATA ++ "/board1.txt",
  %% Heurestic underestimate function
  F = fun({X1, Y1}, {X2, Y2}) -> abs(X1 - X2) + abs(Y1 - Y2) end,
  G = graph:import(File, fun parse_vertex/1),
  {Cost, Path} = a_star:run(G, {1,1}, {6,6}, F),
  io:format("Cost: ~p~nPath: ~p~n", [Cost, Path]).

%% Parses the string that holds a vertex.
-spec parse_vertex(string()) -> my_vertex().
parse_vertex([$(, X, $,, Y, $)]) -> {X - $0, Y - $0}.

%% Dumps a vertex to a string.
-spec dump_vertex(my_vertex()) -> string().
dump_vertex({X, Y}) -> [$(, X + $0, $,, Y + $0, $)].
