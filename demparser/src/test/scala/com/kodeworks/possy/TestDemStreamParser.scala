package com.kodeworks.possy

import org.junit.Test

class TestDemStreamParser {
  @Test
  def testDemStreamParser {
    DemStreamParser.parseDem("C:/dev/src/data/dem/7002_2_10m_z33.dem")
  }
}