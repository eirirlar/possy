package com.kodeworks.possy

import java.nio.channels.AsynchronousFileChannel
import java.nio.file.{Paths, StandardOpenOption}

import scala.io.Codec
import scalaz.stream._
import scalaz.stream.nio.file

object DemStreamParser {
  def parse(path: String): Dem = {
    file.textR(AsynchronousFileChannel.open(Paths.get(path), StandardOpenOption.READ))(Codec.ISO8859)
      .map(DemParser.parse(DemParser.record, _).get)
      .pipe(process1.fold(new DemBuilder())(_(_)))
      .map(_.build)
      .runLast.run.get
  }
}
