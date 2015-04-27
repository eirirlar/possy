package com.kodeworks.possy

import breeze.linalg.DenseMatrix
import org.junit.{Assert, Test}

class TestDem {
  @Test
  def testSnapTo {
    val dem = new SimpleDem("test", 107141, 11141, 101231, 9001, 10, 10, .1f, DenseMatrix.create(0, 0, Array()))
    val g = dem.snapToGrid(101256,9017)
    Assert.assertEquals(3, g._1)
    Assert.assertEquals(2, g._2)
  }
}
