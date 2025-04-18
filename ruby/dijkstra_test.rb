
require_relative 'dijkstra'

# Create a new graph
graph = Graph.new

# Create a more complex graph
graph.add_vertex('S', {'A' => 7, 'B' => 2, 'C' => 3})
graph.add_vertex('A', {'S' => 7, 'B' => 3, 'D' => 4})
graph.add_vertex('B', {'S' => 2, 'A' => 3, 'D' => 4, 'H' => 1})
graph.add_vertex('C', {'S' => 3, 'L' => 2})
graph.add_vertex('D', {'A' => 4, 'B' => 4, 'F' => 5})
graph.add_vertex('H', {'B' => 1, 'F' => 3, 'G' => 2})
graph.add_vertex('G', {'H' => 2, 'E' => 2})
graph.add_vertex('F', {'D' => 5, 'H' => 3})
graph.add_vertex('L', {'C' => 2, 'I' => 4, 'J' => 4})
graph.add_vertex('I', {'L' => 4, 'J' => 6, 'K' => 4})
graph.add_vertex('J', {'L' => 4, 'I' => 6, 'K' => 4})
graph.add_vertex('K', {'I' => 4, 'J' => 4, 'E' => 5})
graph.add_vertex('E', {'G' => 2, 'K' => 5})

# Find and print the shortest path from S to E
path = graph.path_to('S', 'E')
puts "Shortest path from S to E: #{path.join(' -> ')}"

# Print the distance from S to each vertex
distances, _ = graph.shortest_path('S')
puts "\nDistances from S to each vertex:"
distances.sort_by { |vertex, _| vertex }.each do |vertex, distance|
  puts "S -> #{vertex}: #{distance}"
end
