package com.kodeworks.possy

import org.junit.Test

class TestDemStreamParser {
  @Test
  def testDemStreamParser {
    val start = System.currentTimeMillis
    val dem = DemStreamParser.parseDem("C:/dev/src/data/dem/7002_2_10m_z33.dem")
    val end = System.currentTimeMillis
    println(s"time ${end - start} millis")
  }

  @Test
  def testSimpleDemStreamParser {
    val start = System.currentTimeMillis
    val dem = DemStreamParser.parseSimpleDem("C:/dev/src/data/dem/7002_2_10m_z33.dem")
    val end = System.currentTimeMillis
    println(s"time ${end - start} millis")
  }

  @Test
  def testGridParserWriter {
//    val start = System.currentTimeMillis()
//    DemStreamParser.parseWriteGrid("C:/dev/src/data/dem/7002_2_10m_z33.dem", "C:/dev/src/data/dem/5041.compact")
//    println(s"time ${System.currentTimeMillis() - start} millis")

    val s2 = System.currentTimeMillis()
    val grid = DemStreamParser.readGrid("C:/dev/src/data/dem/5041.compact")
    println(grid.size)
    println(s"time ${System.currentTimeMillis() - s2} millis")
  }
}
