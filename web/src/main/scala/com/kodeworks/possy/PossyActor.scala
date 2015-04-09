package com.kodeworks.possy

import java.nio.file.DirectoryStream.Filter
import java.nio.file.{Path, DirectoryStream, Paths, Files}

import akka.actor.Actor
import PossyActor._
import argonaut._, Argonaut._
import com.kodeworks.possy.DemParser._
import org.slf4j.LoggerFactory

import scala.concurrent.Future
import scala.io.{Codec, BufferedSource, Source}

class PossyActor(demPath: String) extends Actor {
  implicit val ec = context.dispatcher

  override def preStart {
    Future {
      listDir(demPath).map(path => {
        val f = Source.fromFile(path.toFile)(Codec.ISO8859)
        val p = DemParser.parseDem(f.mkString)
        f.close()
        println(p.typeA.name)
      })
    }
  }

  def listDir(dir: String): List[Path] = {
    import scala.collection.JavaConverters._
    Files.newDirectoryStream(Paths.get(dir), new Filter[Path] {
      override def accept(entry: Path) = {
        entry.toString.toLowerCase().endsWith(".dem")
      }
    }).asScala.toList
  }

  override def receive = {
    case LoadClosestElevationModel(lat, lng) => sender ! ElevationModel(lat + 5f, lng - 5f, lat - 5f, lng + 5f)
  }
}

object PossyActor {
  val log = LoggerFactory.getLogger(classOf[PossyActor])

  case class LoadClosestElevationModel(
                                        lat: Float,
                                        lng: Float
                                        )

  case class ElevationModel(
                             lat0: Float,
                             lng0: Float,
                             lat1: Float,
                             lng1: Float
                             )

  implicit def LoadClosestElevationModelCodec =
    casecodec2(LoadClosestElevationModel.apply, LoadClosestElevationModel.unapply)("lat", "lng")

  implicit def ElevationModelCodec =
    casecodec4(ElevationModel.apply, ElevationModel.unapply)("lat0", "lng0", "lat1", "lng1")
}
