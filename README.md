erlang-algorithms
=================

About the project
-----------------
The goal of this project is to implement some useful algorithms and data structures in Erlang so as to help anyone who may need them.

*  Currently Implemented Data Structures:
	*  Directed, Undirected, Weighted, Unweighted Graphs
	*  Min / Max Heaps
	*  Union / Find

*  Currently Implemented Algorithms:
	*  BFS
	*  DFS
	*  Dijkstra
	*  Kruskal
	*  Edmonds-Karp
	*  Ford-Fulkerson
   
How to Compile and Run
----------------------
*  Compile the source code and the demo : `make`
*  Run dialyzer : `make dialyze`
*  All of the above : `make all`
*  Run the demo : `make demo`
*  Make edoc : `make edoc`

The file demo.erl contains some functions that demostrate the code in action.
*  demo:min_heaps/0 demonstrates Min Heaps (`erl -pa ebin -eval "demo:min_heaps()" -s init stop`)
*  demo:max_heaps/0 demonstrates Max Heaps (`erl -pa ebin -eval "demo:max_heaps()" -s init stop`)
*  demo:union_find/0 demonstrates Union / Find (`erl -pa ebin -eval "demo:union_find()" -s init stop`)
*  demo:graph/0 demonstrates Dijkstra, BFS and DFS algorithms in a graph (`erl -pa ebin -eval "demo:graph()" -s init stop`)
*  demo:flow/0 demonstrates Edmonds-Karp and Ford-Fulkerson algorithms in a network (`erl -pa ebin -eval "demo:flow()" -s init stop`)

For full documentation check the [site] (http://aggelgian.github.com/erlang-algorithms)

