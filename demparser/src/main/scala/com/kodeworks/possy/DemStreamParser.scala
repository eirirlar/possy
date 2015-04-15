package com.kodeworks.possy

import java.nio.channels.AsynchronousFileChannel
import java.nio.file.{Paths, StandardOpenOption}

import org.slf4j.LoggerFactory

import scala.io.Codec
import scalaz.stream._
import scalaz.stream.nio.file

object DemStreamParser {
  val log = LoggerFactory.getLogger(DemStreamParser.getClass)

  def parse(path: String): Dem = {
    var i = -1
    file.textR(AsynchronousFileChannel.open(Paths.get(path), StandardOpenOption.READ))(Codec.ISO8859)
      .map(s => {
      if (i % 3000 == 0) log.debug(s"Parsed typeB num ${i / 30}, mem ${Runtime.getRuntime.freeMemory() / 1048576L} MB")
      i += 1
      DemParser.parse(DemParser.record, s).get
    })
      .pipe(process1.fold(new DemBuilder())(_(_)))
      .map(_.build)
      .runLast.run.get
  }
}
