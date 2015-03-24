package com.kodeworks.possy

import scala.collection.mutable.ListBuffer

object Possy2 {
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


  def discover(grid: List[List[Int]], target: Int): List[(Int, Int)] = {
    val l: ListBuffer[(Int, Int)] = ListBuffer()
    var i = 0
    while (i < grid.size) {
      var js = grid(i)
      var j = 0
      while (j < js.size) {
        val value = js(j)
        //TODO match on other criteria (range?) and include probability of match
        if (target == value) l.append((i, j))
        j += 1
      }
      i += 1
    }
    l.toList
  }
}
