package com.kodeworks.possy

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object FloydWarshall {
  /**
   * Returns shortest paths between all pairs using Floyd-Warshall algorithm.
   * Nodes are assumed to be enumerated without any holes and enumeration
   * starts from 0.
   *
   * @param nodes the set of vertices
   * @param links the map of edges with costs
   * @return shortest paths between all pairs, including the source and destination
   */
  def allPairsShortestPath(nodes: Set[Int], links: Map[Int, Set[Int]]): Map[Int, Map[Int, Seq[Int]]] = {
    val n = nodes.size
    val inf = Int.MaxValue

    // Initialize distance matrix.
    val ds = Array.fill[Int](n, n)(inf)
    for (i <- 0 until n) ds(i)(i) = 0
    for (i <- links.keys)
      for (j <- links(i))
        ds(i)(j) = 1

    // Initialize next vertex matrix.
    val ns = Array.fill[Int](n, n)(-1)

    // Here goes the magic!
    for (k <- 0 until n; i <- 0 until n; j <- 0 until n)
      if (ds(i)(k) != inf && ds(k)(j) != inf && ds(i)(k) + ds(k)(j) < ds(i)(j)) {
        ds(i)(j) = ds(i)(k) + ds(k)(j)
        ns(i)(j) = k
      }

    // Helper function to carve out paths from the next vertex matrix.
    def extractPath(path: ArrayBuffer[Int], i: Int, j: Int) {
      if (ds(i)(j) == inf) return
      val k = ns(i)(j)
      if (k != -1) {
        extractPath(path, i, k)
        path.append(k)
        extractPath(path, k, j)
      }
    }

    // Extract paths.
    val pss = mutable.Map[Int, Map[Int, Seq[Int]]]()
    for (i <- 0 until n) {
      val ps = mutable.Map[Int, Seq[Int]]()
      for (j <- 0 until n)
        if (ds(i)(j) != inf) {
          val p = new ArrayBuffer[Int]()
          p.append(i)
          if (i != j) {
            extractPath(p, i, j)
            p.append(j)
          }
          ps(j) = p
        }
      pss(i) = ps.toMap
    }

    // Return extracted paths.
    pss.toMap
  }
}
