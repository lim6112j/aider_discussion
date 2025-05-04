require 'minitest/autorun'
require_relative 'bellman_ford'

class BellmanFordTest < Minitest::Test
  def setup
    @graph = Graph.new
    @graph.add_vertex(:a, { b: -1, c: 4 })
    @graph.add_vertex(:b, { c: 3, d: 2, e: 2 })
    @graph.add_vertex(:c, {})
    @graph.add_vertex(:d, { b: 1, c: 5 })
    @graph.add_vertex(:e, { d: -3 })
  end

  def test_shortest_paths
    result = @graph.shortest_paths(:a)
    assert_equal 0, result[:distances][:a]
    assert_equal -1, result[:distances][:b]
    assert_equal 2, result[:distances][:c]
    assert_equal -2, result[:distances][:d]
    assert_equal 1, result[:distances][:e]
  end

  def test_path_reconstruction
    result = @graph.shortest_paths(:a)
    path = @graph.path_to(:a, :d, result[:predecessors])
    assert_equal [:a, :b, :e, :d], path
  end

  def test_negative_cycle_detection
    graph_with_cycle = Graph.new
    graph_with_cycle.add_vertex(:a, { b: 1 })
    graph_with_cycle.add_vertex(:b, { c: -1 })
    graph_with_cycle.add_vertex(:c, { a: -1 })

    assert_nil graph_with_cycle.shortest_paths(:a)
  end
end
