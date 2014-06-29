#! /usr/bin/env ruby

puts "Creating Graphs"
puts "==============="
puts "\ngraph_demo:from_file_default()"
puts `erl -noshell -pa ebin -eval "graph_demo:from_file_default()" -s init stop`
puts "\ngraph_demo:from_file_custom()"
puts `erl -noshell -pa ebin -eval "graph_demo:from_file_custom()" -s init stop`
puts "\ngraph_demo:manual()"
puts `erl -noshell -pa ebin -eval "graph_demo:manual()" -s init stop`

puts "\nBFS Algorithm"
puts "==============="
puts "\nbfs_demo:s1()"
puts `erl -noshell -pa ebin -eval "bfs_demo:s1()" -s init stop`
puts "\nbfs_demo:s2()"
puts `erl -noshell -pa ebin -eval "bfs_demo:s2()" -s init stop`


puts "\nMin Heaps"
puts `erl -noshell -pa ebin -eval "demo:min_heaps()" -s init stop`

puts "Max Heaps"
puts `erl -noshell -pa ebin -eval "demo:max_heaps()" -s init stop`

puts "Union - Find"
puts `erl -noshell -pa ebin -eval "demo:union_find()" -s init stop`

puts "Dijkstra, BFS and DFS algorithms"
puts `erl -noshell -pa ebin -eval "demo:graph()" -s init stop`

puts "Edmonds-Karp and Ford-Fulkerson algorithms"
puts `erl -noshell -pa ebin -eval "demo:flow()" -s init stop`
