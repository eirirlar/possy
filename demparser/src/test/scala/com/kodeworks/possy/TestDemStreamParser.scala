package com.kodeworks.possy

import org.junit.Test

class TestDemStreamParser {
  @Test
  def testDemStreamParser {
    DemStreamParser.parseDem("C:/dev/src/data/dem/7002_2_10m_z33.dem")
  }

  @Test
  def testSimpleDemStreamParser {
    val start = System.currentTimeMillis
    DemStreamParser.parseSimpleDem("C:/dev/src/data/dem/7002_2_10m_z33.dem")
    val end = System.currentTimeMillis
    println(s"time ${end - start} millis")
  }

  @Test
  def testGridParserWriter {
    val start = System.currentTimeMillis()
    DemStreamParser.parseWriteGrid("C:/dev/src/data/dem/7002_2_10m_z33.dem", "C:/dev/src/data/dem/7002_2_10m_z33.compact.dem")
    println(s"time ${System.currentTimeMillis() - start} millis")
  }
}
