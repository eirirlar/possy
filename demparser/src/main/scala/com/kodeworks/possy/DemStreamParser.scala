package com.kodeworks.possy

import java.nio.channels.AsynchronousFileChannel
import java.nio.file.{Paths, StandardOpenOption}

import org.omg.CORBA.MARSHAL
import org.slf4j.LoggerFactory
import scodec.bits.ByteVector

import scala.collection.mutable
import scala.io.{Source, Codec}
import scalaz.concurrent.Task
import scalaz.stream._
import scalaz.stream.nio.file

object DemStreamParser {
  val log = LoggerFactory.getLogger(DemStreamParser.getClass)

  def parseDem(path: String): Dem = {
    val p = new DemParser
    var i = -1
    file.textR(AsynchronousFileChannel.open(Paths.get(path), StandardOpenOption.READ))(Codec.ISO8859)
      .map(s => {
      if (i % 3000 == 0) log.debug(s"Parsed typeB num ${i / 30}, mem ${Runtime.getRuntime.freeMemory() / 1048576L} MB")
      i += 1
      val parsed = p.parse(p.record, s).get
      parsed
    })
      .pipe(process1.fold(new DemBuilder())(_(_)))
      .map(_.build)
      .runLast.run.get
  }

  def recordParser(path: String): Process[Task, Record] = {
    val p = new DemParser
    file.textR(AsynchronousFileChannel.open(Paths.get(path), StandardOpenOption.READ))(Codec.ISO8859)
      .map(p.parse(p.record, _).get)
  }

  def parseSimpleDem(path: String): SimpleDem =
    recordParser(path)
      .pipe(process1.fold(new SimpleDemBuilder())(_(_)))
      .map(_.build)
      .runLast.run.get

  def gridParserWriter(fromPath: String, toPath: String): Process[Task, Unit] =
    recordParser(fromPath).collect {
      case h: RecordTypeBHead => h.elevations.toArray
      case t: RecordTypeBTail => t.elevations.toArray
    }.map(_.map(ByteVector.fromShort(_)).reduce(_ ++ _))
      .to(io.fileChunkW(toPath))

  def parseWriteGrid(fromPath: String, toPath: String) =
    gridParserWriter(fromPath, toPath).run.run

  def gridReader(path: String) =
    Process.constant(4096).toSource
      .through(io.fileChunkR(path))
      .map(_.grouped(2).map(_.toShort()))
      .scan(mutable.ArrayBuilder.make[Short]())((b, s) => {
      b ++= s
      b
    })

  def readGrid(path: String) = {
    gridReader(path).runLast.run.get.result()
  }
}
