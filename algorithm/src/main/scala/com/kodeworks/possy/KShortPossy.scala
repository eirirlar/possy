package com.kodeworks.possy

import breeze.linalg.DenseMatrix
import com.kodeworks.possy.Dijkstra._
import com.kodeworks.possy.KShortPossy._
import MatrixPossy.{discover, discoverNear, combine, distance, split}

import scala.collection.immutable.IndexedSeq
import scala.collection.mutable.ListBuffer

class KShortPossy(grid: DenseMatrix[Short]) {
  val targets = ListBuffer[Short]()
  var lastDiscoveries: IndexedSeq[Int] = null
  val graph = collection.mutable.Map[Int, List[(Double, Int)]]()

  def apply(target: Short) = {
    if (targets.isEmpty) {
      lastDiscoveries = MatrixPossy.discover(grid, target)
    } else {
      val valueMappedToGridIndices = ListBuffer[Int]()
      var j = 0
      while (j < lastDiscoveries.size) {
        val lastDiscovery: Int = lastDiscoveries(j)
        val discoveries: IndexedSeq[Int] = discoverNear(grid, target, lastDiscovery, allowedMovement)
        val distanceList = ListBuffer[(Double, Int)]()

        var k = 0
        while (k < discoveries.size) {
          val discovery: Int = discoveries(k)
          val dist = distance(grid, lastDiscovery, discovery)

          val c = combine(1, valueMappedToGridIndices.size + k)
          distanceList.append((dist.toDouble, c))
          k += 1
        }
        valueMappedToGridIndices ++= discoveries
        val c = combine(1, j)
        graph.put(c, distanceList.toList)
        j += 1
      }
      lastDiscoveries = valueMappedToGridIndices.result.toIndexedSeq
      valueMappedToGridIndices.clear
    }

    //      i = 0
    //      var distanceList = ListBuffer[(Double, Int)]()
    //      while (i < valueMappedToGridIndicesList(0).size) {
    //        val c = combine(0, i)
    //        distanceList.append((0d, c))
    //        i += 1
    //      }
    //      graph.put(start, distanceList.toList)
    //
    //      i = 0
    //      while (i < valueMappedToGridIndicesList(valueMappedToGridIndicesList.size - 1).size) {
    //        val gridIndex: Int = valueMappedToGridIndicesList(valueMappedToGridIndicesList.size - 1)(i)
    //        val c = combine(valueMappedToGridIndicesList.size - 1, i)
    //        graph.put(c, List((0d, end)))
    //        i += 1
    //      }

    graph.put(end, Nil)
    val fringe = List((0d, List(start)))

    val sp: (Double, List[Int]) = shortestPath(graph.toMap, fringe, end, Set())
    //      val shortestPathCoords = shortestPath._2.slice(1, shortestPath._2.size - 1).map(c => {
    //        val s: (Int, Int) = split(c)
    //        grid.rowColumnFromLinearIndex(valueMappedToGridIndicesList(s._1)(s._2))
    //      })

    targets += target
    this
  }
}

object KShortPossy {
  // each new target is required to be within allowedMovement number of rows/cols
  val targetsMax = 100
  // when targets.size reaches targetsMax and a new target is appended, discard oldest target
  val kayMin = 100
  //kay should be kayMin when targets.size == targetsmMax
  val allowedMovement = 10
  // kay should be kayMax when targets is empty, then somehow drop quickly, then slower and slower as more targets are added
  val kayMax = 10000

  val start = 0xffff0000
  val end = 0x0000ffff
}
