package com.kodeworks.possy

import breeze.linalg.DenseMatrix
import org.junit.Test
import TestMatrixPossy._

class TestMatrixPossy {
  @Test
  def testMatrixPossy {
    val s1 = System.currentTimeMillis()
    val dem = DemStreamParser.parseSimpleDem("C:/dev/src/data/dem/7002_2_10m_z33.dem")
    println("parse: " + (System.currentTimeMillis() - s1) + " millis")
    val vs = values(dem.grid, path)
    println(vs)
    val s2 = System.currentTimeMillis()
    MatrixPossy.calculatePath(dem.grid, vs)
    println("calc path: " + (System.currentTimeMillis() - s2) + " millis")
  }
}

object TestMatrixPossy {
  val path = List(
    (0, 0), (1, 1) //, (2, 1), (3, 2), (3, 3), (4, 3), (5, 4), (6, 6), (7, 6), (8, 5)
  )

  def values(grid: DenseMatrix[Short], path: List[(Int, Int)]): List[Short] =
    path.map(rc => grid(rc._1, rc._2))
}