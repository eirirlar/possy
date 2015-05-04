package com.kodeworks.possy

import breeze.linalg.DenseMatrix
import com.kodeworks.possy.Dijkstra._

import scala.collection.immutable.IndexedSeq
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
      val valueMappedToGridIndices = ListBuffer[Int]()

      var j = 0
      while (j < valueMappedToGridIndicesPrev.size) {
        val gridIndexPrev: Int = valueMappedToGridIndicesPrev(j)
        val valueMappedToGridIndicesAroundGridIndexPrev: IndexedSeq[Int] = discoverNear(grid, value, gridIndexPrev, allowedMovement)
        val distanceList = ListBuffer[(Double, Int)]()

        var k = 0
        while (k < valueMappedToGridIndicesAroundGridIndexPrev.size) {
          val gridIndex: Int = valueMappedToGridIndicesAroundGridIndexPrev(k)
          //TODO memo distances?
          val gridIndexDistance = distance(grid, gridIndexPrev, gridIndex)

          val c = combine(i, valueMappedToGridIndices.size + k)
          distanceList.append((gridIndexDistance.toDouble, c))
          k += 1
        }
        valueMappedToGridIndices ++= valueMappedToGridIndicesAroundGridIndexPrev
        val c = combine(i - 1, j)
        graph.put(c, distanceList.toList)
        j += 1
      }
      valueMappedToGridIndicesPrev = valueMappedToGridIndices.result.toIndexedSeq
      valueMappedToGridIndices.clear
      valueMappedToGridIndicesList.append(valueMappedToGridIndicesPrev)
      i += 1
    }

    i = 0
    var distanceList = ListBuffer[(Double, Int)]()
    while (i < valueMappedToGridIndicesList(0).size) {
      val c = combine(0, i)
      distanceList.append((0d, c))
      i += 1
    }
    graph.put(start, distanceList.toList)

    i = 0
    while (i < valueMappedToGridIndicesList(valueMappedToGridIndicesList.size - 1).size) {
      val gridIndex: Int = valueMappedToGridIndicesList(valueMappedToGridIndicesList.size - 1)(i)
      val c = combine(valueMappedToGridIndicesList.size - 1, i)
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

  def discoverNear(grid: DenseMatrix[Short], target: Short, near: Int, allowedMovement: Int): IndexedSeq[Int] = {
    val nearRC: (Int, Int) = grid.rowColumnFromLinearIndex(near)
    val minRow = nearRC._1 - allowedMovement max 0
    val maxRow = nearRC._1 + allowedMovement min grid.rows - 1
    val minCol = nearRC._2 - allowedMovement max 0
    val maxCol = nearRC._2 + allowedMovement min grid.cols - 1
    val nearby: DenseMatrix[Short] = grid(
      minRow to maxRow,
      minCol to maxCol)
    nearby.findAll(_ == target).map(rc =>
      grid.linearIndex(minRow + rc._1, minCol + rc._2)
    )
  }

  def distance(grid: DenseMatrix[Short], fromIndex: Int, toIndex: Int): Int = {
    val xy0 = grid.rowColumnFromLinearIndex(fromIndex)
    val xy1 = grid.rowColumnFromLinearIndex(toIndex)
    val x = xy1._1 - xy0._1
    val y = xy1._2 - xy0._2
    x * x + y * y
  }

  //first 16 bits of k, then 16 bits of i, both must be positive and less than 65535 (because of start and end)
  def combine(i: Int, j: Int): Int = {
    val c = i + (j << 16)
    c
  }

  def split(k: Int): (Int, Int) = {
    (k & 0x0000ffff, (k >> 16) & 0x0000ffff)
  }
}
