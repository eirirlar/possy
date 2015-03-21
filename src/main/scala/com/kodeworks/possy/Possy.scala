package com.kodeworks.possy

import scala.collection.mutable.ListBuffer

object Possy {
  def calculatePath(gridValues: List[List[Int]], path: List[Int]): List[(Int, Int)] = {
    val possiblePaths: List[List[(Int, Int)]] = path.map(p => {
      val l: ListBuffer[(Int, Int)] = ListBuffer()
      var i, j = 0
      while (i < gridValues.size) {
        var js = gridValues(i)
        while (j < js.size) {
          val value = js(j)
          if(p == value) l.append((i, j))
          j += 1
        }
        i += 1
      }
      l.toList
    })
    possiblePaths.map(pp => {
      pp.headOption.getOrElse((-1, -1))
    })
  }
}
