require_relative 'astar'

# Create a grid-based graph for pathfinding
# This is a common use case for A*
def create_grid_graph(width, height)
  graph = Graph.new
  
  # Create a grid of vertices
  (0...height).each do |y|
    (0...width).each do |x|
      vertex_name = "#{x},#{y}"
      graph.add_vertex(vertex_name, [x, y], {})
      
      # Connect to adjacent vertices (if they exist)
      # We'll use 4-way connectivity (no diagonals)
      neighbors = []
      neighbors << ["#{x-1},#{y}", 1] if x > 0                # Left
      neighbors << ["#{x+1},#{y}", 1] if x < width - 1        # Right
      neighbors << ["#{x},#{y-1}", 1] if y > 0                # Up
      neighbors << ["#{x},#{y+1}", 1] if y < height - 1       # Down
      
      # Add edges to neighbors
      neighbors.each do |neighbor, weight|
        graph.add_vertex(neighbor, [neighbor.split(',')[0].to_i, neighbor.split(',')[1].to_i], {}) unless graph.instance_variable_get(:@vertices)[neighbor]
        graph.instance_variable_get(:@vertices)[vertex_name][:edges][neighbor] = weight
      end
    end
  end
  
  # Add some obstacles (walls)
  # For a wall, we'll remove all edges to that vertex
  walls = ["2,1", "2,2", "2,3", "2,4", "4,2", "4,3", "4,4", "4,5", "4,6", "6,4", "7,4", "8,4"]
  
  walls.each do |wall|
    # Remove this vertex's edges
    if graph.instance_variable_get(:@vertices)[wall]
      graph.instance_variable_get(:@vertices)[wall][:edges] = {}
    end
    
    # Remove edges to this vertex from neighbors
    graph.instance_variable_get(:@vertices).each do |vertex_name, vertex_data|
      vertex_data[:edges].delete(wall) if vertex_data[:edges][wall]
    end
  end
  
  return graph
end

# Create a grid graph
grid_graph = create_grid_graph(10, 8)

# Find path from top-left to bottom-right
start = "0,0"
goal = "9,7"

path = grid_graph.a_star(start, goal)

puts "Grid pathfinding from (0,0) to (9,7):"
puts "Path: #{path.join(' -> ')}"
puts "Path length: #{path.length - 1} steps"

# Visualize the grid and path
puts "\nGrid visualization (# = wall, * = path, S = start, G = goal):"
(0...8).each do |y|
  line = ""
  (0...10).each do |x|
    pos = "#{x},#{y}"
    if pos == start
      line += "S"
    elsif pos == goal
      line += "G"
    elsif path.include?(pos)
      line += "*"
    elsif ["2,1", "2,2", "2,3", "2,4", "4,2", "4,3", "4,4", "4,5", "4,6", "6,4", "7,4", "8,4"].include?(pos)
      line += "#"
    else
      line += "."
    end
  end
  puts line
end
