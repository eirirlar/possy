package com.kodeworks.possy

import breeze.linalg.DenseMatrix
import geokonvert.datums.DatumProvider
import geokonvert.scala.Geokonvert
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
    val ksp = new KShortPossy(grid, 20)
    var i = 0
    vs.foreach(v => {
      val s = System.currentTimeMillis()
      ksp(v)
      println("ksp(" + i + ") " + ksp.lastDiscoveries.size + " time " + (System.currentTimeMillis() - s))
      i += 1
    })
    println("time " + (System.currentTimeMillis() - s2) + " millis")
    println("ksps: " + ksp)
  }
}

object TestMatrixPossy {
  val pathX = List(
    (1, 1), (2, 1), (3, 2), (3, 3), (4, 3), (5, 4), (6, 6), (7, 6), (8, 5), (10, 8), (11, 13), (14, 15), (23, 31), (23, 32), (24, 33)
  )
  var path = List(
    (3912,1616),
    (3912,1616),
    (3913,1616),
    (3913,1615),
    (3914,1615),
    (3914,1615),
    (3914,1615),
    (3915,1615),
    (3915,1615),
    (3916,1615),
    (3916,1615),
    (3916,1615),
    (3917,1615),
    (3917,1615),
    (3918,1615),
    (3918,1616),
    (3919,1616),
    (3919,1616),
    (3919,1616),
    (3920,1616),
    (3920,1616),
    (3920,1616),
    (3921,1616)
  )

  def values(grid: DenseMatrix[Short], path: List[(Int, Int)]): List[Short] =
    path.map(rc => grid(rc._1, rc._2))
}