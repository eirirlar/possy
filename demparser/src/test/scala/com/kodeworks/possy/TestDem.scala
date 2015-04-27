package com.kodeworks.possy

import breeze.linalg.DenseMatrix
import org.junit.Test

class TestDem {
  @Test
  def testSnapTo {
    val dem = new SimpleDem("test", 107141, 11141, 101231, 9001, 10, 10, DenseMatrix.create(0, 0, Array()))
    val g = dem.snapToGrid(101256,9017)
    println(g)
  }
}
