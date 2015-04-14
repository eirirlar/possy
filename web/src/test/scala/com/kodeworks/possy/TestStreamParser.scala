package com.kodeworks.possy

import java.nio.charset.StandardCharsets

import org.junit.Test

import scala.collection.mutable.ListBuffer
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
    val builder = new DemBuilder()
    var i = 0
    var last = start
    val converter: Task[Unit] =
      file.textR(toAsyncFileChannel("C:/dev/src/data/dem/7002_2_10m_z33.dem"))(Codec.ISO8859)
        //      fileUnsafeChunkR("C:/dev/src/data/dem/7002_2_10m_z33.dem",1024)
        //        .pipe(iso8859Decode)
        .map(s => {
        val p = DemParser.parse(DemParser.record, s).get
        if (p.isInstanceOf[RecordTypeBHead]) {
          i += 1
          if (i % 100 == 0) {
            println("column " + p.asInstanceOf[RecordTypeBHead].columnNumber + " in " + (System.currentTimeMillis() - last) + " millis, free: " + (Runtime.getRuntime.freeMemory() / 1024L / 1024L) + " MB")
            last = System.currentTimeMillis()
          }
        }
        //builder.apply(p)
        p
      })
    //        .pipe(iso8859Encode)
    //        .to(io.fileChunkW("target/converted.dem", 1024, false)).run
    //TODO repartition and zipwithstate http://stackoverflow.com/questions/28830532/scalaz-stream-how-to-handle-the-header-first-chunks-in-a-different-way-to-t
    converter.run
    val end = System.currentTimeMillis
    val start2 = System.currentTimeMillis
    builder.build()
    val end2 = System.currentTimeMillis
    println("passed time (parse step)" + ((end - start) / 1000) + " seconds")
    println("passed time (build step)" + ((end2 - start2) / 1000) + " seconds")

  }

}
