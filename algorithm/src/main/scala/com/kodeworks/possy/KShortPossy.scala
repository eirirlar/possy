package com.kodeworks.possy

import breeze.linalg.DenseMatrix
import com.kodeworks.possy.KShortPossy._
import com.kodeworks.possy.MatrixPossy.{combine, discoverNear, distance, split}

import scala.collection.immutable.IndexedSeq
import scala.collection.mutable.ListBuffer

class KShortPossy(grid: DenseMatrix[Short], allowedMovement: Int = allowedMovement) {
  val targets = ListBuffer[Short]()
  val calculated = ListBuffer[(Int, Int)]()
  var lastDiscoveries: Seq[(Double, Int)] = null

  def apply(target: Short): KShortPossy = {
    if (targets.isEmpty) {
      lastDiscoveries = MatrixPossy.discover(grid, target).map(0d -> _)
    } else {
      if(target == targets.last) return this
      val graph = collection.mutable.Map[Int, List[(Double, Int)]]()
      val valueMappedToGridIndices = ListBuffer[Int]()
      var distanceToLastDiscoveries = ListBuffer[(Double, Int)]()
      var j = 0
      for ((cost, lastDiscovery) <- lastDiscoveries) {
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
      //TODO proper handling of valueMappedToGridIndices.isEmpty
      if (valueMappedToGridIndices.isEmpty) return this
      graph.put(start, distanceToLastDiscoveries.toList)
      graph.put(end, Nil)
      val ksp: List[(Int, List[Int])] = Gao.kShortestPath(graph.map(a => a._1 -> a._2.map(b => b._1.toInt -> b._2)).toMap, end, start, 5000)
      //TODO consider these factors:
      //"sikksakkhet" of path
      //distance from last point
      //distribution in grid - prefer more distributed paths
      lastDiscoveries = ksp.map(a => a._1.toDouble -> a._2.slice(1 min a._2.size, a._2.size - 1 max 0).map(c => {
        val s = split(c)
        if (0 == s._1) lastDiscoveries(s._2)._2
        else valueMappedToGridIndices(s._2)
      }).last)
        //TODO optimize sort/group/map/sort into one loop
        .sortBy(_._1)
        .groupBy(_._2).toList.map(a => a._2.head._1 -> a._1).sortBy(_._1)
    }
    calculated += grid.rowColumnFromLinearIndex(lastDiscoveries.head._2)
    targets += target
    this
  }

  override def toString = lastDiscoveries.sortBy(_._1).map(i => i._1 -> grid.rowColumnFromLinearIndex(i._2)).mkString(" ")
}

object KShortPossy {
  // each new target is required to be within allowedMovement number of rows/cols
  val targetsMax = 100
  // when targets.size reaches targetsMax and a new target is appended, discard oldest target
  val kayMin = 100
  //kay should be kayMin when targets.size == targetsmMax
  val allowedMovement = 30
  // kay should be kayMax when targets is empty, then somehow drop quickly, then slower and slower as more targets are added
  val kayMax = 10000

  val start = 0xffff0000
  val end = 0x0000ffff
  val endList = List(0d -> end)
}
