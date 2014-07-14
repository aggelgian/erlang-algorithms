#! /usr/bin/env ruby

puts "Creating Graphs"
puts "==============="
puts "\ngraph_demo:from_file_default()"
puts `erl -noshell -pa ebin -eval "graph_demo:from_file_default()" -s init stop`
puts "\ngraph_demo:from_file_custom()"
puts `erl -noshell -pa ebin -eval "graph_demo:from_file_custom()" -s init stop`
puts "\ngraph_demo:manual()"
puts `erl -noshell -pa ebin -eval "graph_demo:manual()" -s init stop`

puts "\nHeaps"
puts "======"
puts "\nheap_demo:min_heap()"
puts `erl -noshell -pa ebin -eval "heap_demo:min_heap()" -s init stop`
puts "\nheap_demo:max_heap()"
puts `erl -noshell -pa ebin -eval "heap_demo:max_heap()" -s init stop`

puts "\nUnion Find"
puts "==========="
puts "\nunion_find_demo:uf1()"
puts `erl -noshell -pa ebin -eval "union_find_demo:uf1()" -s init stop`

puts "\nBFS Algorithm"
puts "=============="
puts "\nbfs_demo:s1()"
puts `erl -noshell -pa ebin -eval "bfs_demo:s1()" -s init stop`
puts "\nbfs_demo:s2()"
puts `erl -noshell -pa ebin -eval "bfs_demo:s2()" -s init stop`

puts "\nDFS Algorithm"
puts "=============="
puts "\ndfs_demo:s1()"
puts `erl -noshell -pa ebin -eval "dfs_demo:s1()" -s init stop`
puts "\ndfs_demo:s2()"
puts `erl -noshell -pa ebin -eval "dfs_demo:s2()" -s init stop`

puts "\nDijkstra Algorithm"
puts "==================="
puts "\ndijkstra_demo:s1()"
puts `erl -noshell -pa ebin -eval "dijkstra_demo:s1()" -s init stop`
puts "\ndijkstra_demo:s2()"
puts `erl -noshell -pa ebin -eval "dijkstra_demo:s2()" -s init stop`

puts "\nKruskal Algorithm"
puts "=================="
puts "\nkruskal_demo:s1()"
puts `erl -noshell -pa ebin -eval "kruskal_demo:s1()" -s init stop`
puts "\nkruskal_demo:s2()"
puts `erl -noshell -pa ebin -eval "kruskal_demo:s2()" -s init stop`

puts "\nEdmonds-Karp and Ford-Fulkerson algorithms"
puts "==========================================="
puts "\nflow_demo:s1()"
puts `erl -noshell -pa ebin -eval "flow_demo:s1()" -s init stop`

puts "\nA* Search Algorithm"
puts "==================="
puts "\na_star_demo:b1()"
puts `erl -noshell -pa ebin -eval "a_star_demo:b1()" -s init stop`


