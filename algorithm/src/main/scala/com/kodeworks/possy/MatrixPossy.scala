package com.kodeworks.possy

import breeze.linalg.DenseMatrix
import com.kodeworks.possy.Dijkstra._

import scala.collection.immutable.IndexedSeq
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object MatrixPossy {
  val maxAllowedMovement = 10

  //TODO support dynamic version, same graph but with nodes removed in beginning or added in end
  def calculatePath(grid: DenseMatrix[Short], values: List[Short], allowedMovement: Int = maxAllowedMovement): List[(Int, Int)] = {
    val start = 0xffff0000
    val end = 0x0000ffff
    val graph = collection.mutable.Map[Int, List[(Double, Int)]]()
    var valueMappedToGridIndicesPrev: IndexedSeq[Int] = discover(grid, values(0))
    val valueMappedToGridIndicesList = ListBuffer[IndexedSeq[Int]](valueMappedToGridIndicesPrev)

    var i = 1
    while (i < values.size) {
      val value = values(i)
      val valueMappedToGridIndices = Set.newBuilder[Int]

      //TODO listbuilder instead
      //val valueMappedToGridIndices: List[Int] = discover(grid, value).toList
      //append built list after while loop j
      //valueMappedToGridIndicesList.append(valueMappedToGridIndices)

      var j = 0
      while (j < valueMappedToGridIndicesPrev.size) {
        val gridIndexPrev: Int = valueMappedToGridIndicesPrev(j)
        val rc: (Int, Int) = grid.rowColumnFromLinearIndex(gridIndexPrev)
        val aroundGridIndexPrev: DenseMatrix[Short] = grid(
          rc._1 - allowedMovement max 0 to (rc._1 + allowedMovement min grid.rows - 1),
          rc._2 - allowedMovement max 0 to (rc._2 + allowedMovement min grid.cols - 1))
        val valueMappedToGridIndicesAroundGridIndexPrev: IndexedSeq[Int] = discover(aroundGridIndexPrev, value)
        //TODO discover here instead, append to valueMappedToGridIndices
        valueMappedToGridIndices ++= valueMappedToGridIndicesAroundGridIndexPrev
        // slice around gridIndexPrev allowedMovement size
        val distanceList = ListBuffer[(Double, Int)]()

        var k = 0
        //TODO k should iterate through slice around gridIndexPrev instead
        while (k < valueMappedToGridIndicesAroundGridIndexPrev.size) {
          val gridIndex: Int = valueMappedToGridIndicesAroundGridIndexPrev(k)
          //TODO memo distances?
          val gridIndexDistance = distance(grid, gridIndexPrev, gridIndex)
          //first 16 bits of k, then 16 bits of i, both must be positive and less than 65535 (because of start and end)
          val c = combine(i, k)
          if (c == 0 || c == end || c == start) throw new AssertionError("c == 0 || c == end || c == start")
          distanceList.append((gridIndexDistance.toDouble, c))
          k += 1
        }
        val c = combine(i - 1, j)
        if (c == end || c == start) throw new AssertionError(" c == end || c == start")
        graph.put(c, distanceList.toList)
        j += 1
      }
      valueMappedToGridIndicesPrev = valueMappedToGridIndices.result.toIndexedSeq
      valueMappedToGridIndices.clear
      i += 1
    }

    i = 0
    var distanceList = ListBuffer[(Double, Int)]()
    while (i < valueMappedToGridIndicesList(0).size) {
      val c = combine(0, i)
      if (c == end || c == start) throw new AssertionError("c == end || c == start")
      distanceList.append((0d, c))
      i += 1
    }
    graph.put(start, distanceList.toList)

    i = 0
    while (i < valueMappedToGridIndicesList(valueMappedToGridIndicesList.size - 1).size) {
      val gridIndex: Int = valueMappedToGridIndicesList(valueMappedToGridIndicesList.size - 1)(i)
      val c = combine(valueMappedToGridIndicesList.size - 1, i)
      if ((valueMappedToGridIndicesList.size != 1 && c == 0) || c == end || c == start) throw new AssertionError("c == 0 || c == end || c == start")
      graph.put(c, List((0d, end)))
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
