package com.kodeworks.possy

import com.kodeworks.possy.TestGao._
import org.junit.Test

class TestGao {
  @Test
  def testGao {
    var ksp = Gao.kShortestPath(lookup, -2, -1, 3)
    println("te=0,pn=0\n" + ksp.mkString("\n"))
    ksp = Gao.kShortestPath(lookup, -2, -1, 3, true)
    println("\nte=1,pn=0\n" + ksp.mkString("\n"))
    ksp = Gao.kShortestPath(lookup, -2, -1, 3, true, true)
    println("\nte=1,pn=1\n" + ksp.mkString("\n"))
    ksp = Gao.kShortestPath(lookup, -2, -1, 3, false, true)
    println("\nte=0,pn=1\n" + ksp.mkString("\n"))
  }
}

object TestGao {

  val lookup: Map[Int, List[(Int, Int)]] = Map(
    -1 -> List((0.0, 2), (0.0, 8)),
    0 -> List((0.0, -2)),
    1 -> List((0.0, -2)),
    7 -> List((0.0, -2)),
    2 -> List((1.0, 5)),
    5 -> List((5.0, 0), (4.0, 3), (2.0, 7)),
    8 -> List((1.0, 5)),
    3 -> List((2.0, -2)),
    -2 -> Nil
  ).map(e => e._1 -> e._2.map(cn => cn._1.toInt -> cn._2))

  val grid = List(
    List(1, 0, 2),
    List(1, 0, 9),
    List(0, 1, 2)
  )
  val path = List(
    (2, 0), (2, 1), (1, 2) // 2, 9, 1 -> 2, 5, 7 -> [2, 8], [5], [0, 3, 7]
  )
}
