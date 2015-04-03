package com.kodeworks.possy

import scala.collection.mutable.ListBuffer
import Dijkstra._

object Possy {
  //TODO support dynamic version, same graph but with nodes removed in beginning or added in end
  def calculatePath(grid: List[List[Int]], values: List[Int]): (Double, List[(Int, Int)]) = {
    val start = 0xffff0000
    val end = 0x0000ffff
    val graph = collection.mutable.Map[Int, List[(Double, Int)]]()
    var valueMappedToGridIndicesPrev: List[Int] = discover(grid, values(0))
    val valueMappedToGridIndicesList = ListBuffer[List[Int]](valueMappedToGridIndicesPrev)

    var i = 1
    while (i < values.size) {
      val value = values(i)
      val valueMappedToGridIndices: List[Int] = discover(grid, value)
      valueMappedToGridIndicesList.append(valueMappedToGridIndices)

      var j = 0
      while (j < valueMappedToGridIndicesPrev.size) {
        val gridIndexPrev: Int = valueMappedToGridIndicesPrev(j)
        val distanceList = ListBuffer[(Double, Int)]()

        var k = 0
        while (k < valueMappedToGridIndices.size) {
          val gridIndex: Int = valueMappedToGridIndices(k)
          //TODO memo distances?
          val gridIndexDistance = distance(grid.size, gridIndexPrev, gridIndex)
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
      val s = split(c)
      toGridCoords(grid.size, valueMappedToGridIndicesList(s._1)(s._2))
    })
    (shortestPath._1, shortestPathCoords)
  }

  def discover(grid: List[List[Int]], target: Int): List[Int] = {
    val l: ListBuffer[Int] = ListBuffer()
    var i = 0
    while (i < grid.size) {
      var js = grid(i)
      var j = 0
      while (j < js.size) {
        val value = js(j)
        //TODO match on other criteria (range?) and include probability of match
        if (target == value) l.append(toGridIndex(grid.size, i, j))
        j += 1
      }
      i += 1
    }
    l.toList
  }

  def distance(width: Int, fromIndex: Int, toIndex: Int): Int = {
    val xy0 = toGridCoords(width, fromIndex)
    val xy1 = toGridCoords(width, toIndex)
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

  def toGridIndex(gridWidth: Int, x: Int, y: Int): Int = x * gridWidth + y

  def toGridCoords(gridWidth: Int, gridIndex: Int): (Int, Int) = (gridIndex % gridWidth, gridIndex / gridWidth)
}
