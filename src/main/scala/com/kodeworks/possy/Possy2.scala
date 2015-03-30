package com.kodeworks.possy

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Possy2 {
  /*
      def calculatePath(grid: List[List[Int]], values: List[Int]): List[(Int, Int)] = {
        val pps = ListBuffer[ListBuffer[(Int, Int)]]()
        var i = 0
        while (i < values.size) {
          val value: Int = values(i)
          val locations: List[(Int, Int)] = discover(grid, value)
          //TODO empty locations
          if (pps.isEmpty) {
            pps.appendAll(locations.map(ListBuffer(_)))
          } else {
            var ppsCopy = pps.map(_.clone)
            pps.foreach(_.append(locations.head))
            val numCopies = locations.size - 1
            var j = 0
            while (j < numCopies) {
              pps.appendAll(ppsCopy.map(pp => {
                val ppCopy = pp.clone
                ppCopy.append(locations(j+1))
                ppCopy
              }))
              j+=1
            }
          }
          i += 1
        }

        println("possible paths " + pps)
        List()
      }
    */
  def calculatePath(grid: List[List[Int]], values: List[Int]): List[(Int, Int)] = {
    val start = -1
    val end = -2
    //lookup: Map[Int, List[(Double, Int)]], fringe: List[Path[Int]], dest: Key,
    val graph = collection.mutable.Map[Int, List[(Double, Int)]]() //, fringe: List[Path[Int]], dest: Key,
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
          distanceList.append((gridIndexDistance.toDouble, combine(i, k))) //first 16 bits of k, then 16 bits of i, both must be positive and less than 65536
        }
        graph.put(combine(i, j), distanceList.toList)
        j += 1
      }
      valueMappedToGridIndicesPrev = valueMappedToGridIndices
      i += 1
    }
    //TODO prepend to graph first list
    //TODO append to graph last list
    //TODO find dijkstra 2nd param
    //TODO run dijkstra
    //TODO split dijkstra output and look up in valueMappedToGridIndicesList to find grid indices
    List()
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
        if (target == value) l.append((i * grid.size + j))
        j += 1
      }
      i += 1
    }
    l.toList
  }

  def distance(gridWidth: Int, fromGridIndex: Int, toGridIndex: Int): Int = {
    val x0 = fromGridIndex / gridWidth
    val y0 = fromGridIndex % gridWidth
    val x1 = toGridIndex / gridWidth
    val y1 = toGridIndex % gridWidth
    val x = x1 - x0
    val y = y1 - y0
    x * x + y * y
  }

  def combine(i: Int, j: Int): Int = {
    i + (j << 16)
  }

  def split(k: Int): (Int, Int) = {
    (k & 0x0000ffff, (k >> 16) & 0x0000ffff)
  }

}
