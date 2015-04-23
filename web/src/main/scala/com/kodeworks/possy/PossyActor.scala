package com.kodeworks.possy

import java.io.File
import java.nio.file.DirectoryStream.Filter
import java.nio.file.{Files, Path, Paths}

import akka.actor.Actor
import argonaut.Argonaut._
import com.kodeworks.possy.PossyActor._
import geokonvert.datums.{DatumUtils, DatumProvider}
import geokonvert.scala.Geokonvert
import org.slf4j.LoggerFactory

import scala.concurrent.Future
import scala.io.{Codec, Source}
import akka.pattern.pipe
import Model._

class PossyActor(demPath: String) extends Actor {
  var dems: Map[(Float, Float), Dem] = Map()

  implicit val ec = context.dispatcher

  override def preStart {
    val zelf = self
    Future {
      val dir: List[Path] = listDir(demPath)
      log.debug("Going to parse {} .dem files:\n{}", dir.size, dir.map(_.toFile.getName).mkString("\n"))
      Future.sequence(dir.map(path => {
        Future {
          val file: File = path.toFile
          log.debug("Parsing {}", file.getName)
          val start = System.currentTimeMillis
          val p = DemStreamParser.parseDem(file.getCanonicalPath)
          val end = System.currentTimeMillis
          log.debug(s"Parsed  ${file.getName} in ${(end - start) / 1000L} seconds")
          p
        }
      })) pipeTo zelf
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
    case LatLng(lat, lng) => {
      if (dems.nonEmpty) {
        val u = Geokonvert.transformToUTM(lat, lng, DatumProvider.WGS84, true)
        val s = dems.keys.toList.sortBy(k => distancePow(k._1, k._2, u.E.toFloat, u.N.toFloat))
        val a = dems(s.head).typeA
        val ne = Geokonvert.transformFromUTM(a.northingOfNE, a.eastingOfNE, 33, DatumProvider.WGS84)
        val sw = Geokonvert.transformFromUTM(a.northingOfSE, a.eastingOfNW, 33, DatumProvider.WGS84)
        sender ! ElevationModel(sw.getX.toFloat, sw.getY.toFloat, ne.getX.toFloat, ne.getY.toFloat)
      }
      else sender ! ElevationModel(lat + 5f, lng - 5f, lat - 5f, lng + 5f)
    }
    case dems: List[Dem] => {
      this.dems = dems.map(dem => {
        log.debug(s"${dem.typeA.name}\neastingOfNE ${dem.typeA.eastingOfNE}\neastingOfNW ${dem.typeA.eastingOfNW}\neastingOfSE ${dem.typeA.eastingOfSE}\neastingOfSW ${dem.typeA.eastingOfSW}\n" +
          s"northingOfNE ${dem.typeA.northingOfNE}\nnorthingOfNW ${dem.typeA.northingOfNW}\nnorthingOfSE ${dem.typeA.northingOfSE}\nnorthingOfSW ${dem.typeA.northingOfSW}")
        (dem.typeA.eastingOfNW + (dem.typeA.eastingOfNE - dem.typeA.eastingOfNW) / 2f, dem.typeA.northingOfSE + (dem.typeA.northingOfNE - dem.typeA.northingOfSE) / 2f) -> dem
      }).toMap
      log.debug("Got list of dems")
    }
  }
}

object PossyActor {
  val log = LoggerFactory.getLogger(classOf[PossyActor])


  case class ElevationModel(
                             lat0: Float,
                             lng0: Float,
                             lat1: Float,
                             lng1: Float
                             )


  implicit def ElevationModelCodec =
    casecodec4(ElevationModel.apply, ElevationModel.unapply)("lat0", "lng0", "lat1", "lng1")

  def distancePow(x0: Float, y0: Float, x1: Float, y1: Float): Float = {
    val x = x1 - x0
    val y = y1 - y0
    x * x + y * y
  }
}
