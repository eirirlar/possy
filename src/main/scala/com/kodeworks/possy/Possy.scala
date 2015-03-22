package com.kodeworks.possy

import scala.collection.mutable.ListBuffer

object Possy {
  def calculatePathMin(gridValues: List[List[Int]], path: List[Int]) = {
    List()
  }

  def calculatePath(gridValues: List[List[Int]], path: List[Int]): List[(Int, Int)] = {
    val possiblePaths: List[List[(Int, Int)]] = this.possiblePaths(gridValues, path)
    val distancesPowed: List[List[List[Int]]] = possiblePaths.sliding(2).map((pp: List[List[(Int, Int)]]) => {
      val pp0 = pp(0)
      val pp1 = pp(1)
      pp0.map((p0: (Int, Int)) => {
        pp1.map((p1: (Int, Int)) => {
          val x = p1._1 - p0._1
          val y = p1._2 - p0._2
          x * x + y * y
        })
      })
    }).toList
    println("distancesPowed " + distancesPowed)
    possiblePaths.map(pp => {
      pp.headOption.getOrElse((-1, -1))
    })
  }

  def possiblePaths(gridValues: List[List[Int]], path: List[Int]): List[List[(Int, Int)]] = {
    val possiblePaths: List[List[(Int, Int)]] = path.map(p => {
      val l: ListBuffer[(Int, Int)] = ListBuffer()
      var i = 0
      while (i < gridValues.size) {
        var js = gridValues(i)
        var j = 0
        while (j < js.size) {
          val value = js(j)
          if (p == value) l.append((i, j))
          j += 1
        }
        i += 1
      }
      l.toList
    })
    possiblePaths
  }
}
