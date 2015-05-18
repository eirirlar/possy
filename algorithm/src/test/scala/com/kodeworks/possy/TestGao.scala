package com.kodeworks.possy

import org.junit.{Assert, Test}
import TestGao._

class TestGao {
  @Test
  def testGao {
    val nodeIdToIndex = lookup.keys.zipWithIndex.toMap
    val nodeIndexToId = nodeIdToIndex.map(_.swap)
    val lookup2 = lookup.map(kv => nodeIdToIndex(kv._1) -> kv._2)
    Gao.kShortestPath(lookup2,)
    println("nodeIdToIndex " + nodeIdToIndex)
    println("nodeIndexToId " + nodeIndexToId)
    //    Gao.kShortestPath(lookup,-2, )
  }
}

object TestGao {

  val lookup: Map[Int, List[(Double, Int)]] = Map(
    -1 -> List((0.0, 2), (0.0, 8)),
    0 -> List((0.0, -2)),
    1 -> List((0.0, -2)),
    7 -> List((0.0, -2)),
    2 -> List((1.0, 5)),
    5 -> List((5.0, 0), (4.0, 3), (2.0, 7)),
    8 -> List((1.0, 5)),
    -2 -> Nil
  )

  val grid = List(
    List(1, 0, 2),
    List(1, 0, 9),
    List(0, 1, 2)
  )
  val path = List(
    (2, 0), (2, 1), (1, 2)
  )
}
