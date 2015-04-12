package com.kodeworks.possy

import java.io.{BufferedInputStream, FileInputStream}
import java.nio.channels.AsynchronousFileChannel
import java.nio.charset.StandardCharsets
import java.nio.file.{StandardOpenOption, Paths}

import scodec.bits.ByteVector

import scalaz.concurrent.Task
import scalaz.stream._
import scalaz.stream.process1._

object StreamUtil {
  def fileUnsafeChunkR(f: String, bufferSize: Int = 4096): Channel[Task, Array[Byte], Array[Byte]] =
    io.unsafeChunkR(new BufferedInputStream(new FileInputStream(f), bufferSize))

  def toAsyncFileChannel(path: String) = AsynchronousFileChannel.open(Paths.get(path), StandardOpenOption.READ)

  val iso8859Encode: Process1[String, ByteVector] =
    lift(s => ByteVector.view(s.getBytes(StandardCharsets.ISO_8859_1)))

//  val iso8859Decode: Process1[ByteVector, String] = {
//    lift(b => new String(b.toArray, StandardCharsets.ISO_8859_1))
//  }

  val iso8859Decode:Process1[Array[Byte], String] = {
    lift(b => new String(b, StandardCharsets.ISO_8859_1))
  }
}
