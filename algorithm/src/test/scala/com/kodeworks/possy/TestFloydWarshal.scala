package com.kodeworks.possy

import org.junit.Test

class TestFloydWarshal {

  @Test
  def testAllPairsShortestPath {
//    FloydWarshall.allPairsShortestPath(nodes,)

  }
}

object TestFloydWarshal {
  val nodes = Set(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

  val grid = List(
    List(1, 0, 2),
    List(1, 0, 9),
    List(0, 1, 2)
  )
  val path = List(
    (2, 0), (2, 1), (1, 2) // 0, 1, 9
    /*
    target, coords, nodeids
    0: 0,1  1,1  2,0 // 2  5  7
    1: 0,0  1,0  2,1 // 1  4  8
    9: 1,2           // 6
     */
  )
}
