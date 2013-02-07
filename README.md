erlang-algorithms
=================

About the project
-----------------
The goal of this project is to implement some useful algorithms and dat structures in Erlang so as to help anyone who may need them.

Currently Implemented Data Structures:
	Directed, Undirected, Weighted, Unweighted Graphs
	Min / Max Heaps

Currently Implemented Algorithmss:
	BFS
	DFS
	Dijkstra

Graph Specs
-----------
The file that contains the graph must have the following format.

*  The 1st line will consist of four terms separeted by a white space.
   *  1st Term: Positive Integer N that denotes the number of vertices.
   *  2nd Term: Positive Integer M that denotes the number of edges.
   *  3rd Term: Atom `directed` or `undirected` that denotes the type of the graph.
   *  4th Term: Atom `unweighted` or `d` or `f` that denotes the type of the edge weights.
      *  `unweighted` is for an  unweighted graph.
      *  `d` is for decimal integer weights.
      *  `f` is for floating point number weights in proper Erlang syntax.

*  The next M lines will consist of the edge descriptions. 
   Each line will contain three terms : U, V, W. 
   This will denote an edge from U to V with W weight. 
   
How to Compile and Run
----------------------
*  Compile the source code and the demo : `make`
*  Run dialyzer : `make dialyze`
*  All of the above : `make all`

The file demo.erl contains some functions that demostrate the code in action.
*  demo:min_heaps/0 demonstrates Min Heaps (`erl -pa ebin -eval "demo:min_heaps()" -s init stop`)
*  demo:max_heaps/0 demonstrates Max Heaps (`erl -pa ebin -eval "demo:max_heaps()" -s init stop`)
*  demo:graph/0 demonstrates Dijkstra, BFS and DFS algorithms in a graph (`erl -pa ebin -eval "demo:graph()" -s init stop`)

