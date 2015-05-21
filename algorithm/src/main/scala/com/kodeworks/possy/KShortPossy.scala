package com.kodeworks.possy

import breeze.linalg.DenseMatrix
import com.kodeworks.possy.Dijkstra._
import com.kodeworks.possy.KShortPossy._
import com.kodeworks.possy.MatrixPossy.{combine, split, discoverNear, distance}

import scala.collection.immutable.IndexedSeq
import scala.collection.mutable.ListBuffer

class KShortPossy(grid: DenseMatrix[Short], allowedMovement:Int = allowedMovement) {
  val targets = ListBuffer[Short]()
  var lastDiscoveries: IndexedSeq[(Double, Int)] = null

  def apply(target: Short) = {
    val graph = collection.mutable.Map[Int, List[(Double, Int)]]()
    if (targets.isEmpty) {
      lastDiscoveries = MatrixPossy.discover(grid, target).map(0d -> _)
    } else {
      val valueMappedToGridIndices = ListBuffer[Int]()
      var distanceToLastDiscoveries = ListBuffer[(Double, Int)]()
      var j = 0
      for ((cost, lastDiscovery) <- lastDiscoveries) {
        val (cost, lastDiscovery) = lastDiscoveries(j)
        val discoveries: IndexedSeq[Int] = discoverNear(grid, target, lastDiscovery, allowedMovement)
        val distanceToDiscoveries = ListBuffer[(Double, Int)]()

        var k = 0
        for (discovery <- discoveries) {
          val dist = distance(grid, lastDiscovery, discovery)
          val c = combine(1, valueMappedToGridIndices.size + k)
          graph.put(c, endList)
          distanceToDiscoveries.append((dist.toDouble, c))
          k += 1
        }
        valueMappedToGridIndices ++= discoveries
        val c = combine(0, j)
        graph.put(c, distanceToDiscoveries.toList)
        distanceToLastDiscoveries.append(cost -> c)
        j += 1
      }
      graph.put(start, distanceToLastDiscoveries.toList)
      graph.put(end, Nil)
      val ksp: List[(Int, List[Int])] = Gao.kShortestPath(graph.map(a => a._1 -> a._2.map(b => b._1.toInt -> b._2)).toMap, end, start,1000)
      lastDiscoveries = ksp.map(a => a._1.toDouble -> a._2.slice(1 min a._2.size, a._2.size - 1 max 0).map(c => {
        val s = split(c)
        if (0 == s._1) lastDiscoveries(s._2)._2
        else valueMappedToGridIndices(s._2)
      }).last).toIndexedSeq
    }
    targets += target
    this
  }

  override def toString = lastDiscoveries.map(i => grid.rowColumnFromLinearIndex(i._2)).mkString(" ")
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
  val endList = List(0d -> end)
}
