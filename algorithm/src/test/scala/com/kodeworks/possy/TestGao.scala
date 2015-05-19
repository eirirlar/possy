package com.kodeworks.possy

import com.kodeworks.possy.TestGao._
import org.junit.Test

class TestGao {
  @Test
  def testGao {
    val nodeIdToIndex = lookup.keys.zipWithIndex.toMap
    val nodeIndexToId = nodeIdToIndex.map(_.swap)
    val lookup2: Map[Int, List[(Int, Int)]] = lookup.map(kv => nodeIdToIndex(kv._1) -> kv._2.map(kv => kv._1.toInt -> {
      val x = nodeIdToIndex(kv._2)
      x
    }))
    val kspIndexed = Gao.kShortestPath(lookup2, nodeIdToIndex(-2), nodeIdToIndex(-1), 2)
    val ksp = kspIndexed.map(_.map(nodeIndexToId(_)).toList).toList

    println("ksp " + ksp.mkString("\n"))
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
    3 -> List((2.0, -2)),
    -2 -> Nil
  )

  val grid = List(
    List(1, 0, 2),
    List(1, 0, 9),
    List(0, 1, 2)
  )
  val path = List(
    (2, 0), (2, 1), (1, 2) // 2, 9, 1 -> 2, 5, 7 -> [2, 8], [5], [0, 3, 7]
  )
}
