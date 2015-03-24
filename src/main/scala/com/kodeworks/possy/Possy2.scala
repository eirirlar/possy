package com.kodeworks.possy

import scala.collection.mutable.ListBuffer

object Possy2 {
  def discover(grid: List[List[Int]], target: Int) = {
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
