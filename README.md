erlang-algorithms
=================


Graph Specs
-----------
The file that contains the graph must have the following format.

*  The 1st line will consist of four terms separeted by a white space.
   *  1st Term: Positive Integer N that denotes the number of vertices.
   *  2nd Term: Positive Integer M that denotes the number of edges.
   *  3rd Term: Atom `directed` or  `undirected` that denotes the type of the graph.
   *  4th Term: Atom `unweighted` or `d` or `u` or `f` that denotes the type of the edge weights.
      *  `unweighted` is for an  unweighted graph.
      *  `d` is for decimal integer weights.
      *  `f` is for floating point number weights in proper Erlang syntax.

*  The next M lines will consist of the edge descriptions. 
   Each line will contain three terms : U, V, W. 
   This will denote an edge from U to V with W weight. 
   
How to Compile and Run
----------------------
*  You can compile everything from within the interpreter with
  ```erlang
  c(graph),c(graph_lib),c(dijkstra),c(bfs),c(dfs),c(e).
  ```
  The file e.erl contains a demo. You can view it with ```erlang e:run()```

*  Or you can compile with `make` from the Unix Shell and run the demo with `erl -noshell -s e run -s init stop`
