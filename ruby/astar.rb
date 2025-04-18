class Graph
  def initialize
    @vertices = {}
  end

  # Add a vertex to the graph
  def add_vertex(name, position = nil, edges = {})
    @vertices[name] = {
      position: position,  # For heuristic calculation (e.g., x,y coordinates)
      edges: edges         # Connections to other vertices
    }
  end

  # Calculate the heuristic distance between two vertices
  # This is the estimated cost from current to goal
  def heuristic(current, goal)
    # If positions are provided, use Euclidean distance
    if @vertices[current][:position] && @vertices[goal][:position]
      x1, y1 = @vertices[current][:position]
      x2, y2 = @vertices[goal][:position]
      return Math.sqrt((x2 - x1)**2 + (y2 - y1)**2)
    else
      # If no positions, use 0 (falls back to Dijkstra's algorithm)
      return 0
    end
  end

  # Find the shortest path using A* algorithm
  def a_star(start, goal)
    # Return empty path if start or goal doesn't exist
    return [] unless @vertices[start] && @vertices[goal]
    
    # Return single node path if start is the goal
    return [start] if start == goal

    # The set of discovered nodes that need to be evaluated
    open_set = [start]
    
    # For each node, which node it can most efficiently be reached from
    came_from = {}
    
    # For each node, the cost of getting from start to that node
    g_score = {}
    @vertices.keys.each { |vertex| g_score[vertex] = Float::INFINITY }
    g_score[start] = 0
    
    # For each node, the estimated total cost from start to goal passing through that node
    f_score = {}
    @vertices.keys.each { |vertex| f_score[vertex] = Float::INFINITY }
    f_score[start] = heuristic(start, goal)
    
    while !open_set.empty?
      # Get the node in open_set with the lowest f_score
      current = open_set.min_by { |vertex| f_score[vertex] }
      
      # If we've reached the goal, reconstruct and return the path
      if current == goal
        path = []
        while current
          path.unshift(current)
          current = came_from[current]
        end
        return path
      end
      
      # Remove current from open_set
      open_set.delete(current)
      
      # Check each neighbor of current
      @vertices[current][:edges].each do |neighbor, weight|
        # Tentative g_score is the g_score of current plus the cost to move to neighbor
        tentative_g_score = g_score[current] + weight
        
        # If this path to neighbor is better than any previous one, record it
        if tentative_g_score < g_score[neighbor]
          came_from[neighbor] = current
          g_score[neighbor] = tentative_g_score
          f_score[neighbor] = g_score[neighbor] + heuristic(neighbor, goal)
          
          # Add neighbor to open_set if it's not there
          open_set << neighbor unless open_set.include?(neighbor)
        end
      end
    end
    
    # If we get here, there's no path from start to goal
    return []
  end
end

# Example usage
if __FILE__ == $PROGRAM_NAME
  # Create a graph with positions for heuristic calculation
  graph = Graph.new
  
  # Add vertices with their positions (x,y) and edges {neighbor => weight}
  graph.add_vertex('A', [0, 0], {'B' => 7, 'C' => 9, 'F' => 14})
  graph.add_vertex('B', [7, 0], {'A' => 7, 'C' => 10, 'D' => 15})
  graph.add_vertex('C', [5, 5], {'A' => 9, 'B' => 10, 'D' => 11, 'F' => 2})
  graph.add_vertex('D', [12, 10], {'B' => 15, 'C' => 11, 'E' => 6})
  graph.add_vertex('E', [20, 5], {'D' => 6, 'F' => 9})
  graph.add_vertex('F', [10, 15], {'A' => 14, 'C' => 2, 'E' => 9})
  
  # Find shortest path from A to E using A*
  path = graph.a_star('A', 'E')
  
  puts "Shortest path from A to E using A*: #{path.join(' -> ')}"
  
  # Compare with a path that doesn't use positions (like Dijkstra's)
  graph_no_pos = Graph.new
  graph_no_pos.add_vertex('A', nil, {'B' => 7, 'C' => 9, 'F' => 14})
  graph_no_pos.add_vertex('B', nil, {'A' => 7, 'C' => 10, 'D' => 15})
  graph_no_pos.add_vertex('C', nil, {'A' => 9, 'B' => 10, 'D' => 11, 'F' => 2})
  graph_no_pos.add_vertex('D', nil, {'B' => 15, 'C' => 11, 'E' => 6})
  graph_no_pos.add_vertex('E', nil, {'D' => 6, 'F' => 9})
  graph_no_pos.add_vertex('F', nil, {'A' => 14, 'C' => 2, 'E' => 9})
  
  path_no_pos = graph_no_pos.a_star('A', 'E')
  
  puts "Shortest path without heuristic: #{path_no_pos.join(' -> ')}"
end
