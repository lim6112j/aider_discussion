class Graph
  def initialize
    @vertices = {}
  end

  # Add a vertex to the graph
  def add_vertex(name, edges = {})
    @vertices[name] = edges
  end

  # Find shortest paths from source using Bellman-Ford algorithm
  # Returns { distances: {}, predecessors: {} } or nil if negative cycle exists
  def shortest_paths(source)
    distances = {}
    predecessors = {}
    
    # Initialize distances
    @vertices.each_key do |vertex|
      distances[vertex] = Float::INFINITY
      predecessors[vertex] = nil
    end
    distances[source] = 0

    # Relax all edges |V| - 1 times
    (@vertices.size - 1).times do
      @vertices.each do |vertex, edges|
        edges.each do |neighbor, weight|
          if distances[vertex] + weight < distances[neighbor]
            distances[neighbor] = distances[vertex] + weight
            predecessors[neighbor] = vertex
          end
        end
      end
    end

    # Check for negative-weight cycles
    @vertices.each do |vertex, edges|
      edges.each do |neighbor, weight|
        if distances[vertex] + weight < distances[neighbor]
          return nil # Negative cycle detected
        end
      end
    end

    { distances: distances, predecessors: predecessors }
  end

  # Reconstruct path from source to target
  def path_to(source, target, predecessors)
    path = []
    current = target

    while current != source && current
      path.unshift(current)
      current = predecessors[current]
    end

    path.unshift(source) if current == source
    path.empty? ? nil : path
  end
end
