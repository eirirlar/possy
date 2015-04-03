package com.kodeworks.possy
import org.junit.Test

import scala.io.{Codec, BufferedSource, Source}

class TestDemParser {
  @Test
  def testDemParser{
    val f: BufferedSource = Source.fromFile("C:/dev/src/temp/6602_1_10m_z33.dem")(Codec.ISO8859)
    val dem: Dem = DemParser.parseDem(f.bufferedReader())
    f.close()
    println(dem)
  }
}
