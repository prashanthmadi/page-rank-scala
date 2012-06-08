package com.prashanth.graph
import scala.collection.mutable.HashMap
import java.util.ArrayList

object Graph {
  val default_vertex_rank = 0.1;
  val vertex = new HashMap[String, Double];
  val in_edge = new HashMap[String, ArrayList[String]];
  val out_edge = new HashMap[String, ArrayList[String]]

  // adding vertices
  def addVertex(vertex_name: String, vertex_rank: Double) {
    vertex.put(vertex_name, vertex_rank)
  }

  /*
   * adding edges
   * in_edge has all incoming links
   * out_edge has all out going links
   */

  def addEdge(from_vertex: String, to_vertex: String) {
    if (!vertex.contains(from_vertex))
      vertex.put(from_vertex, default_vertex_rank)
    if (!vertex.contains(to_vertex))
      vertex.put(to_vertex, default_vertex_rank)

    // in edges
    var in_edge_links = in_edge.getOrElse(to_vertex, new ArrayList[String])
    in_edge_links.add(from_vertex)
    in_edge.put(to_vertex, in_edge_links)

    // out edges
    var out_edge_links = out_edge.getOrElse(from_vertex, new ArrayList[String])
    out_edge_links.add(to_vertex)
    out_edge.put(from_vertex, out_edge_links)

  }
  
  
  
}