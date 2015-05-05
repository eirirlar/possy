package com.kodeworks.possy

import breeze.linalg.DenseMatrix
import org.junit.Test
import TestMatrixPossy._

class TestMatrixPossy {
  @Test
  def testMatrixPossy {
    val s1 = System.currentTimeMillis()
    val grid = new DenseMatrix(5041, 5041, DemStreamParser.readGrid("C:/dev/src/data/dem/5041.compact"))
    println("parse: " + (System.currentTimeMillis() - s1) + " millis")
    val vs = values(grid, path)
    println("    path : " + path)
    println("pathVals : " + vs)
    val s2 = System.currentTimeMillis()
    val calcPath: List[(Int, Int)] = MatrixPossy.calculatePath(grid, vs)
    println("calcPath : " + calcPath + " " + (System.currentTimeMillis() - s2) + " millis")
  }

  @Test
  def testKShortPossy {
    val s1 = System.currentTimeMillis()
    val grid = new DenseMatrix(5041, 5041, DemStreamParser.readGrid("C:/dev/src/data/dem/5041.compact"))
    println("parse: " + (System.currentTimeMillis() - s1) + " millis")
    val vs = values(grid, path)
    println("    path : " + path)
    println("pathVals : " + vs)
    val s2 = System.currentTimeMillis()
    val ksp = new KShortPossy(grid)
    vs.foreach(ksp(_))
    println("ksp targets " + ksp.targets + " time " + (System.currentTimeMillis() - s2) + " millis")
  }
}

object TestMatrixPossy {
  val path = List(
    (1, 1), (2, 1), (3, 2), (3, 3), (4, 3), (5, 4), (6, 6), (7, 6), (8, 5)
  )

  def values(grid: DenseMatrix[Short], path: List[(Int, Int)]): List[Short] =
    path.map(rc => grid(rc._1, rc._2))
}