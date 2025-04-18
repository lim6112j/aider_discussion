
class Graph
  def initialize
    @vertices = {}
  end

  # Add a vertex to the graph
  def add_vertex(name, edges = [])
    @vertices[name] = edges
  end

  # Find the shortest path from source to all other vertices
  def shortest_path(source)
    distances = {}
    previous = {}
    nodes = []

    # Initialize distances and previous
    @vertices.each_key do |vertex|
      if vertex == source
        distances[vertex] = 0
      else
        distances[vertex] = Float::INFINITY
      end
      previous[vertex] = nil
      nodes << vertex
    end

    # Main algorithm
    while nodes.any?
      # Find vertex with minimum distance
      current = nodes.min_by { |vertex| distances[vertex] }

      # Break if we're processing unreachable vertices
      break if distances[current] == Float::INFINITY

      # Remove current from unvisited nodes
      nodes.delete(current)

      # Check each neighbor of current
      @vertices[current].each do |neighbor, weight|
        alt = distances[current] + weight

        # If we found a shorter path to neighbor
        if alt < distances[neighbor]
          distances[neighbor] = alt
          previous[neighbor] = current
        end
      end
    end

    [distances, previous]
  end

  # Reconstruct the path from source to target
  def path_to(source, target)
    distances, previous = shortest_path(source)

    return [] if distances[target] == Float::INFINITY

    path = []
    current = target

    # Work backwards from target to source
    while current
      path.unshift(current)
      current = previous[current]
    end

    path
  end
end

# Example usage
if __FILE__ == $PROGRAM_NAME
  graph = Graph.new

  # Add vertices with their edges [neighbor, weight]
  graph.add_vertex('A', {'B' => 7, 'C' => 9, 'F' => 14})
  graph.add_vertex('B', {'A' => 7, 'C' => 10, 'D' => 15})
  graph.add_vertex('C', {'A' => 9, 'B' => 10, 'D' => 11, 'F' => 2})
  graph.add_vertex('D', {'B' => 15, 'C' => 11, 'E' => 6})
  graph.add_vertex('E', {'D' => 6, 'F' => 9})
  graph.add_vertex('F', {'A' => 14, 'C' => 2, 'E' => 9})

  # Find shortest path from A to E
  path = graph.path_to('A', 'E')

  puts "Shortest path from A to E: #{path.join(' -> ')}"

  # Get all shortest paths from A
  distances, _ = graph.shortest_path('A')
  puts "\nShortest distances from A:"
  distances.each do |vertex, distance|
    puts "A -> #{vertex}: #{distance}"
  end
end
