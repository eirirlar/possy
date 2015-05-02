package com.kodeworks.possy

import java.awt.geom.Point2D
import java.io.File
import java.nio.file.DirectoryStream.Filter
import java.nio.file.{Files, Path, Paths}

import akka.actor.Actor
import akka.pattern.pipe
import argonaut.Argonaut._
import com.kodeworks.possy.Model._
import com.kodeworks.possy.PossyActor._
import geokonvert.datums.DatumProvider
import geokonvert.scala.Geokonvert
import org.slf4j.LoggerFactory

import scala.concurrent.Future

class PossyActor(demPath: String) extends Actor {
  var dems: Map[(Float, Float), SimpleDem] = Map()

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
          val p = DemStreamParser.parseSimpleDem(file.getCanonicalPath)
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

  def getClosestDem(lat: Float, lng: Float): Option[SimpleDem] = {
    if (dems.nonEmpty) {
      val u = Geokonvert.transformToUTM(lat, lng, DatumProvider.WGS84, true)
      val s = dems.keys.toList.sortBy(k => distancePow(k._1, k._2, u.E.toFloat, u.N.toFloat))
      Some(dems(s.head))
    } else None
  }

  override def receive = {
    case LatLng(lat, lng) => {
      getClosestDem(lat, lng) match {
        case Some(s: SimpleDem) => {
          val nw = Geokonvert.transformFromUTM(s.northingOfNE, s.eastingOfSW, 33, DatumProvider.WGS84)
          val ne = Geokonvert.transformFromUTM(s.northingOfNE, s.eastingOfNE, 33, DatumProvider.WGS84)
          val sw = Geokonvert.transformFromUTM(s.northingOfSW, s.eastingOfSW, 33, DatumProvider.WGS84)
          val se = Geokonvert.transformFromUTM(s.northingOfSW, s.eastingOfNE, 33, DatumProvider.WGS84)
          sender ! ElevationModel(
            p2dToDouble(nw),
            p2dToDouble(ne),
            p2dToDouble(sw),
            p2dToDouble(se))
        }
        case _ => {
//          sender ! ElevationModel(lat - 5f, lng - 5f, lat + 5f, lng + 5f)
        }
      }
    }

    case GetClosestDem(LatLng(lat, lng)) => {
      sender ! getClosestDem(lat, lng)
    }

    case dems: List[SimpleDem] => {
      this.dems = dems.map(dem => {
        log.debug(s"${dem.name}\neastingOfNE ${dem.eastingOfNE}\neastingOfSW ${dem.eastingOfSW}\n" +
          s"northingOfNE ${dem.northingOfNE}\nnorthingOfSW ${dem.northingOfSW}")
        (dem.eastingOfSW + (dem.eastingOfNE - dem.eastingOfSW) / 2f, dem.northingOfSW + (dem.northingOfNE - dem.northingOfSW) / 2f) -> dem
      }).toMap
      log.debug("Got list of dems")
    }
  }
}

object PossyActor {
  val log = LoggerFactory.getLogger(classOf[PossyActor])


  case class ElevationModel(
                             nw: (Float, Float),
                             ne: (Float, Float),
                             sw: (Float, Float),
                             se: (Float, Float))


  implicit def ElevationModelCodec =
    casecodec4(ElevationModel.apply, ElevationModel.unapply)("nw", "ne", "sw", "se")

  def distancePow(x0: Float, y0: Float, x1: Float, y1: Float): Float = {
    val x = x1 - x0
    val y = y1 - y0
    x * x + y * y
  }

  def p2dToDouble(p2d: Point2D.Double): (Float, Float) =
    (p2d.x.toFloat, p2d.y.toFloat)
}
