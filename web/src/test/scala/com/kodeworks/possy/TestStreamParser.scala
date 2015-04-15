package com.kodeworks.possy

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

import org.junit.Test
import scodec.bits.ByteVector

import scala.collection.mutable.ListBuffer
import scala.io.Codec
import scala.language.{implicitConversions, reflectiveCalls}
import scalaz.concurrent.Task
import scalaz.stream.Process._
import scalaz.stream._
import scalaz.stream.nio._
import StreamUtil._

import scalaz.stream.process1._


class TestStreamParser {
  @Test
  def testBasic {
    val start = System.currentTimeMillis
    val builder = new DemBuilder()
    var i = 0
    var last = start



    val converter  =
      file.textR(toAsyncFileChannel("C:/dev/src/data/dem/7002_2_10m_z33.dem"))(Codec.ISO8859)
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
        .runLog.run.toList

    //.pipe(demBuilderProcess).runLast
    //.pipe(process1.fold(builder)(_(_)))
    //.map(_.toString).pipe(iso8859Encode).to(io.fileChunkW("target/converted.dem", 1024, false)).run
    val end = System.currentTimeMillis
    println("passed time (parse step)" + ((end - start) / 1000) + " seconds")

  }

}
