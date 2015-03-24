package com.kodeworks.possy

import org.junit.Test

class TestPossy {
  import TestPossy._
  @Test
  def test1 {

    val calculatedPath = Possy.calculatePath(grid, values)

    println("path: " + path)
    println("pathElevations " + values)
    println("calculatedPath " + calculatedPath)
  }

  @Test
  def test2 {
    val calculatedPath = Possy2.calculatePath(grid, values)

    println("path: " + path)
    println("pathElevations " + values)
    println("calculatedPath " + calculatedPath)
  }

}

object TestPossy {
  val grid = List(
    List(1, 0, 2, 3, 4, 5, 8, 7, 6, 5, 4),
    List(1, 0, 9, 3, 4, 5, 5, 6, 7, 5, 4),
    List(0, 1, 2, 2, 3, 4, 6, 7, 8, 7, 6),
    List(3, 2, 2, 2, 2, 2, 3, 4, 3, 4, 3),
    List(4, 4, 3, 3, 2, 2, 3, 4, 3, 4, 3),
    List(5, 5, 5, 6, 6, 5, 4, 6, 8, 8, 3),
    List(4, 5, 6, 7, 7, 7, 6, 7, 6, 7, 8),
    List(7, 6, 5, 6, 5, 5, 4, 3, 5, 6, 6),
    List(5, 5, 6, 7, 7, 7, 4, 3, 2, 3, 4),
    List(4, 4, 4, 5, 6, 5, 6, 7, 4, 5, 1)
  )
  val path = List(
    (0, 0), (1, 1), (2, 1), (3, 2) , (3, 3), (4, 3)//, (5, 4), (6, 6), (7, 6), (8, 5)
  )
  val values = path.map(p => grid(p._2)(p._1))

}
