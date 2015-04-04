package com.kodeworks.possy

import org.junit.{Assert, Test}

import scala.io.{Codec, BufferedSource, Source}
import scala.util.Success

class TestDemParser {
  @Test
  def testDemParser {
    val f: BufferedSource = Source.fromFile("C:/dev/src/temp/6602_1_10m_z33.dem")(Codec.ISO8859)
    val dem: Dem = DemParser.parseDem(f.bufferedReader())
    f.close()
    println(dem)
  }

  @Test
  def testAnyN: Unit = {
    val s = "     1" //5 space, 1
    val p = DemParser.parse(DemParser.anyN(6), s)
    p.get
  }
}
