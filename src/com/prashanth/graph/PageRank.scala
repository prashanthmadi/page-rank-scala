package com.prashanth.graph
import java.util.ArrayList
import scala.collection.mutable.HashMap
import scala.util.Random

object PageRank extends App {

  def manualGraph() {
    Graph.addVertex("A", 0.4)
    Graph.addVertex("B", 0.3)
    Graph.addVertex("C", 0.3)
    Graph.addVertex("D", 0.3)
    Graph.addVertex("E", 0.3)
    Graph.addEdge("A", "B")
    Graph.addEdge("A", "C")
    Graph.addEdge("A", "D")
    Graph.addEdge("A", "E")
    Graph.addEdge("C", "B")
    Graph.addEdge("C", "D")
    Graph.addEdge("D", "E")
    Graph.addEdge("B", "E")
    Graph.addEdge("C", "E")
    Graph.addEdge("H", "I")
  }

  println("start--------- Creating Graph")
  randomGraph()
  // manualGraph()
  println("end---------")

  /*
   * Creating a Random Graph 
   */
  def randomGraph() {
    val alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    val graph_vertices = alphabet.split("")
    println(graph_vertices.length)
    for (each_vertex <- graph_vertices; if (each_vertex != "")) {
      //  println(each_vertex)
      Graph.addVertex(each_vertex, Math.random)
      for (i <- 1 to 5) {
        if (Math.random > 0.3) {
          Graph.addEdge(each_vertex, graph_vertices(Random.nextInt(25)))
          //        println(each_vertex+" - "+graph_vertices(Random.nextInt(25)))
        }
      }
    }
  }

  println(Graph.vertex)
  println("-----------")
  println(Graph.in_edge)
  println("-----------")

  val old_rank = Graph.vertex
  pageRankConverge(old_rank)

  // checking for convergence
  def pageRankConverge(old_page_rank: HashMap[String, Double]) {
    val new_page_rank = pageRankAlgoImpl(old_page_rank, Graph.in_edge, Graph.out_edge)

    def checkthis(): Boolean = {
      for (vertex <- old_page_rank.keySet) {
        var old_value = Math.round(old_page_rank(vertex) * 100) / 100.0d
        var new_value = Math.round(new_page_rank(vertex) * 100) / 100.0d
        if (Math.abs(new_value - old_value) > 0.04) {
          println("repeat")
          return false

        }
      }
      return true
    }
    if (checkthis() == true) {
      println("Old Page Rank is : " + old_page_rank)
      println("New Page Rank is : " + new_page_rank)
    } else {
      pageRankConverge(new_page_rank)
    }
  }

  // pagerank algorithm implementation
  def pageRankAlgoImpl(vertices: HashMap[String, Double], in_edge: HashMap[String, ArrayList[String]], out_edge: HashMap[String, ArrayList[String]]): HashMap[String, Double] = {
    val new_vertex = new HashMap[String, Double];
    val n_value = vertices.keySet.size
    val d_value = 0.85
    for (vertex <- vertices.keySet) {
      var right_value: Double = 0;
      var vertex_new_rank: Double = 0;
      if (in_edge.contains(vertex)) {
        val it = in_edge(vertex).listIterator()
        while (it.hasNext()) {
          var incoming_edge = it.next()
          var ratio = vertices(incoming_edge) / out_edge(incoming_edge).size()
          right_value += ratio;
          //     println(vertex + " - " + incoming_edge + " - " + out_edge(incoming_edge).size())
        }
        vertex_new_rank = ((1 - d_value) / n_value) + (d_value * right_value)

      } else {
        vertex_new_rank = Graph.default_vertex_rank
      }
      //      println(vertex + " - " + vertex_new_rank)
      //     println("-----")
      new_vertex.put(vertex, vertex_new_rank)
    }
    return new_vertex
  }

}