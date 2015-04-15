package com.kodeworks.possy

import java.nio.channels.AsynchronousFileChannel
import java.nio.file.{Paths, StandardOpenOption}

import org.junit.Test

import scala.io.Codec
import scala.language.{implicitConversions, reflectiveCalls}
import scalaz.stream._
import scalaz.stream.nio._


class TestStreamParser {
  @Test
  def testBasic {
    val start = System.currentTimeMillis
    val builder = new DemBuilder()
    var i = 0
    var last = start

    val converter =
      file.textR(AsynchronousFileChannel.open(Paths.get("C:/dev/src/data/dem/7002_2_10m_z33.dem"), StandardOpenOption.READ))(Codec.ISO8859)
        .map(s => {
        val p = DemParser.parse(DemParser.record, s).get
        if (p.isInstanceOf[RecordTypeBHead]) {
          i += 1
          if (i % 100 == 0) {
            println("column " + p.asInstanceOf[RecordTypeBHead].columnNumber + " in " + (System.currentTimeMillis() - last) + " millis, free: " + (Runtime.getRuntime.freeMemory() / 1024L / 1024L) + " MB")
            last = System.currentTimeMillis()
          }
        }
        p
      })
        .pipe(process1.fold(builder)(_(_))).map(_.build)
    val end = System.currentTimeMillis
    converter.runLast.run.get
    println("passed time " + ((end - start) / 1000) + " seconds")
  }
}
