package com.kodeworks.possy

import Dijkstra._
import org.junit.Test

class TestDijkstra {
  @Test
  def testDijkstra {
    val lookup: Map[(Int, Int), List[(Double, (Int, Int))]] = Map(
      (-1,-1) -> List((0.0, (2,0)), (0.0, (2,2))),
      (0,0)-> List((0.0, (-2,-2))),
      (0,1)-> List((0.0, (-2,-2))),
      (1,2)-> List((0.0, (-2,-2))),
      (2, 0) -> List((1.0, (2, 1))),
      (2, 1) -> List((5.0, (0, 0)), (4.0, (1, 0)), (2.0, (1, 2))),
      (2, 2) -> List((1.0, (2,1))),
      (-2,-2) -> Nil
    )
    val start = System.currentTimeMillis()
    val res = shortestPath[(Int, Int)](lookup, List((0d, List((-1,-1)))), (-2,-2), Set())
    val time = System.currentTimeMillis() - start
    println("dijkstra time: " + time)
    println(res)

    val lookup2: Map[Int, List[(Double, Int)]] = Map(
      -1 -> List((0.0, 2), (0.0, 8)),
      0 -> List((0.0, -2)),
      1-> List((0.0, -2)),
      7-> List((0.0, -2)),
      2 -> List((1.0, 5)),
      5 -> List((5.0, 0), (4.0, 3), (2.0, 7)),
      8 -> List((1.0, 5)),
      -2 -> Nil
    )
    val start2 = System.currentTimeMillis()
    val res2 = shortestPath[Int](lookup2, List((0d, List(-1))), -2, Set())
    val time2 = System.currentTimeMillis() - start2
    println("dijkstra time: " + time2)
    println(res2)
  }
}

object TestDijkstra {
  val grid = List(
    List(1, 0, 2),
    List(1, 0, 9),
    List(0, 1, 2)
  )
  val path = List(
    (2, 0), (2, 1), (1, 2)
  )
}
