#! /usr/bin/env ruby

puts "Min Heaps"
puts `erl -noshell -pa ebin -eval "demo:min_heaps()" -s init stop`

puts "Max Heaps"
puts `erl -noshell -pa ebin -eval "demo:max_heaps()" -s init stop`

puts "Union - Find"
puts `erl -noshell -pa ebin -eval "demo:union_find()" -s init stop`

puts "Dijkstra, BFS and DFS algorithms"
puts `erl -noshell -pa ebin -eval "demo:graph()" -s init stop`

puts "Edmonds-Karp and Ford-Fulkerson algorithms"
puts `erl -noshell -pa ebin -eval "demo:flow()" -s init stop`
