package com.kodeworks.possy

import breeze.linalg.DenseMatrix
import com.kodeworks.possy.Dijkstra._

import scala.collection.immutable.IndexedSeq
import scala.collection.mutable.ListBuffer

object MatrixPossy {
  val maxAllowedMovement = 10

  //TODO support dynamic version, same graph but with nodes removed in beginning or added in end
  def calculatePath(grid: DenseMatrix[Short], values: List[Short], allowedMovement:Int = maxAllowedMovement): List[(Int, Int)] = {
    val start = 0xffff0000
    val end = 0x0000ffff
    val graph = collection.mutable.Map[Int, List[(Double, Int)]]()
    var valueMappedToGridIndicesPrev: List[Int] = discover(grid, values(0)).toList
    val valueMappedToGridIndicesList = ListBuffer[List[Int]](valueMappedToGridIndicesPrev)

    var i = 1
    while (i < values.size) {
      val value = values(i)
      //TODO listbuilder instead
      val valueMappedToGridIndices: List[Int] = discover(grid, value).toList
      //append built list after while loop j
      valueMappedToGridIndicesList.append(valueMappedToGridIndices)

      var j = 0
      while (j < valueMappedToGridIndicesPrev.size) {
        val gridIndexPrev: Int = valueMappedToGridIndicesPrev(j)
        //TODO discover here instead, append to valueMappedToGridIndices
        // slice around gridIndexPrev allowedMovement size
        val distanceList = ListBuffer[(Double, Int)]()

        var k = 0
        //TODO k should iterate through slice around gridIndexPrev instead
        while (k < valueMappedToGridIndices.size) {
          val gridIndex: Int = valueMappedToGridIndices(k)
          //TODO memo distances?
          val gridIndexDistance = distance(grid, gridIndexPrev, gridIndex)
          //first 16 bits of k, then 16 bits of i, both must be positive and less than 65535 (because of start and end)
          distanceList.append((gridIndexDistance.toDouble, combine(i, k)))
          k += 1
        }
        graph.put(combine(i - 1, j), distanceList.toList)
        j += 1
      }
      valueMappedToGridIndicesPrev = valueMappedToGridIndices
      i += 1
    }

    i = 0
    var distanceList = ListBuffer[(Double, Int)]()
    while (i < valueMappedToGridIndicesList(0).size) {
      distanceList.append((0d, combine(0, i)))
      i += 1
    }
    graph.put(start, distanceList.toList)

    i = 0
    while (i < valueMappedToGridIndicesList(valueMappedToGridIndicesList.size - 1).size) {
      val gridIndex: Int = valueMappedToGridIndicesList(valueMappedToGridIndicesList.size - 1)(i)
      graph.put(combine(valueMappedToGridIndicesList.size - 1, i), List((0d, end)))
      i += 1
    }

    graph.put(end, Nil)
    val fringe = List((0d, List(start)))

    val shortestPath: (Double, List[Int]) = dijkstra(graph.toMap, fringe, end, Set())
    val shortestPathCoords = shortestPath._2.slice(1, shortestPath._2.size - 1).map(c => {
      val s: (Int, Int) = split(c)
      grid.rowColumnFromLinearIndex(valueMappedToGridIndicesList(s._1)(s._2))
    })
    shortestPathCoords
  }

  def discover(grid: DenseMatrix[Short], target: Short): IndexedSeq[Int] = {
    grid.findAll(_ == target).map(rc => grid.linearIndex(rc._1, rc._2))
  }

  def distance(grid: DenseMatrix[Short], fromIndex: Int, toIndex: Int): Int = {
    val xy0 = grid.rowColumnFromLinearIndex(fromIndex)
    val xy1 = grid.rowColumnFromLinearIndex(toIndex)
    val x = xy1._1 - xy0._1
    val y = xy1._2 - xy0._2
    x * x + y * y
  }

  def combine(i: Int, j: Int): Int = {
    i + (j << 16)
  }

  def split(k: Int): (Int, Int) = {
    (k & 0x0000ffff, (k >> 16) & 0x0000ffff)
  }

  //
  //  def toGridIndex(gridWidth: Int, x: Int, y: Int): Int = x * gridWidth + y
  //
  //  def toGridCoords(gridWidth: Int, gridIndex: Int): (Int, Int) = (gridIndex % gridWidth, gridIndex / gridWidth)
}
