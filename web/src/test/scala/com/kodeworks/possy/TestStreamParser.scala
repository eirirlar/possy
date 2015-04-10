package com.kodeworks.possy

import org.junit.Test

import scala.io.Codec
import scala.language.{implicitConversions, reflectiveCalls}
import scalaz.concurrent.Task
import scalaz.stream._
import scalaz.stream.nio._
import StreamUtil._


class TestStreamParser {
  @Test
  def testBasic {
    val start = System.currentTimeMillis
    val converter: Task[Unit] = file.textR(toAsyncFileChannel("C:/dev/src/data/dem/7002_2_10m_z33.dem"))(Codec.ISO8859)
      .map(s => DemParser.parse(DemParser.record, s).get.toString)
      .pipe(iso8859Encode)
      .to(io.fileChunkW("target/converted.dem", 1024, false)).run

    // at the end of the universe...
    val u: Unit = converter.run
    val end = System.currentTimeMillis
    println("passed time " + ((end - start) / 1000) + " seconds")

  }

}
