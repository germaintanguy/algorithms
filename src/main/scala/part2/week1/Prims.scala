package part2.week1

import java.util

import scala.io.Source

/**
 * Created by germaintanguy on 14/04/15.
 *
 * Source: COURSERA - Algorithms: Design and Analysis, Part 2 - Programming Assignment #1
 *
 * QUESTION 3 :
 * In this programming problem you'll code up Prim's minimum spanning tree algorithm.
 * Download the text file here. This file describes an undirected graph with integer edge costs.
 *
 * It has the format :
 *
 * [number_of_nodes] [number_of_edges]
 *  [one_node_of_edge_1] [other_node_of_edge_1] [edge_1_cost] 
 * [one_node_of_edge_2] [other_node_of_edge_2] [edge_2_cost]
 *  ... 
 *
 * For example, the third line of the file is "2 3 -8874", indicating that there is an edge
 * connecting vertex #2 and vertex #3 that has cost -8874. You should NOT assume that edge
 * costs are positive, nor should you assume that they are distinct.
 *
 * Your task is to run Prim's minimum spanning tree algorithm on this graph.

 * IMPLEMENTATION NOTES: This graph is small enough that the straightforward O(mn)
 * time implementation of Prim's algorithm should work fine.
 *
 */
object Prims extends App {

  /**
   *
   * @param path of the file witch contains data
   * @return an undirected graph represented by adjacency matrix, hashSet of vertices
   */
  def readData(path: String): (Array[Array[Double]], util.HashSet[Int], Int) = {

    val dataSet = Source.fromFile(path).getLines.toList
    val nbVertex = dataSet.head.split(" ")(0).toInt
    val nbEdge = dataSet.head.split(" ")(1).toInt
    val graph = Array.fill(nbVertex, nbVertex)(Double.MaxValue)
    val vertices: util.HashSet[Int] = new util.HashSet()

    val firstVertex = dataSet(1).split(" ")(0).toInt - 1

    dataSet.drop(1).foreach {
      j =>
        val arr = j.split(" ")
        val vertex1 = arr(0).toInt - 1
        val vertex2 = arr(1).toInt - 1
        val edge = arr(2).toDouble
        graph(vertex1)(vertex2) = edge // because undirected graph
        graph(vertex2)(vertex1) = edge // because undirected graph

        vertices.add(vertex1)
        vertices.add(vertex2)
    }
    graph.toList.foreach(l => println(l.toList))
    (graph, vertices, firstVertex)
  }

  def compute(graph: Array[Array[Double]], vertices: util.HashSet[Int], firstVertex: Int): Double = {
    println(firstVertex)
    var costTree: Double = 0
    val v_visited: util.HashSet[Int] = new util.HashSet()

    var v = firstVertex
    v_visited.add(v)

    while (v_visited != vertices) {

      // All edges not visited yet (vertex1,vertex2, weight)
      val edgesNotVisited: Array[(Int, Int, Double)] =
        graph.zipWithIndex.filter {
          case (v_neighbors: Array[Double], v_id: Int)
          => v_visited.contains(v_id)
        }.flatMap {
          case (v_neighbors: Array[Double], v_id: Int)
          => v_neighbors.zipWithIndex.filter {
            case (w: Double, u_id: Int) => !v_visited.contains(u_id)
          }.map {
            case (w: Double, u_id: Int) => (v_id, u_id, w)
          }
        }

      // Let min = (v, u) be the cheapest edge of G with v ∈ v_visited, u ∈/ v_visited.
      val min: (Int, Int, Double) = edgesNotVisited.minBy(e => e._3)

      v = min._2
      v_visited.add(v)
      costTree += min._3
    }
    costTree
  }


  val (graph, vertices, startPoint) = readData("data/part2/week1/edges.txt")
  println(compute(graph, vertices, startPoint))
  println("End")
}
