package com.kodeworks.possy

import akka.actor.{Actor, ActorRef}
import akka.pattern.ask
import akka.util.Timeout
import com.kodeworks.possy.Boot.possy
import com.kodeworks.possy.Model._
import com.kodeworks.possy.PathCalculator._
import geokonvert.datums.DatumProvider
import geokonvert.scala.Geokonvert
import org.slf4j.LoggerFactory

import scala.concurrent.Future
import scala.concurrent.duration._

class PathCalculator extends Actor {
  implicit def ctx = context.dispatcher

  implicit val timeout = Timeout(500 seconds)

  var path = List[(Float, Float)]()
  var dem: SimpleDem = null

  override def receive = {
    case CalcPath(ll@LatLng(lat, lng)) => checkNoDem(ll) onSuccess {
      case sender => sender ! calcPath(lat, lng)
    }
    case GetElevation(l@LatLng(lat, lng)) => checkNoDem(l) onSuccess {
      case sender => sender ! elevation(lat, lng).toFloat / dem.resolutionZ
    }
    case ResetCalc => {
      log.debug("reset calc")
      path = Nil
      dem = null
      sender ! Ok
    }
    case Some(d: SimpleDem) => this.dem = d
  }

  def checkNoDem(ll: LatLng): Future[ActorRef] = {
    val zender = sender
    if (null == dem) possy ? GetClosestDem(ll) map {
      case Some(d: SimpleDem) => {
        this.dem = d
        zender
      }
    }
    else Future.successful(zender)
  }

  def calcPath(lat: Float, lng: Float): (Long, List[(Float, Float)]) = {
    log.debug("lat lng")
    path = (lat, lng) :: path
    val elevs = elevations
    val start = System.nanoTime()
    val calcedPath: List[(Int, Int)] = MatrixPossy.calculatePath(dem.grid, elevs, 10)
    val end = System.nanoTime()
    val time = (end - start) / 1000000L
    val llCalcedPath: List[(Float, Float)] = calcedPath.map(gc => {
      val n = gc._1 * dem.resolutionY + dem.northingOfSW
      val e = gc._2 * dem.resolutionX + dem.eastingOfSW
      val ll = Geokonvert.transformFromUTM(n, e, 33, DatumProvider.WGS84)
      (ll.x.toFloat, ll.y.toFloat)
    })
    time -> llCalcedPath
  }

  def elevation(lat: Float, lng: Float): Short = {
    val u = Geokonvert.transformToUTM(lat, lng, DatumProvider.WGS84, true)
    val (n, e) = dem.snapToGrid(u.N.toInt, u.E.toInt)
    dem.grid(n, e)
  }

  def elevations: List[Short] =
    path.map(ll => elevation(ll._1, ll._2))
}

object PathCalculator {
  val log = LoggerFactory.getLogger(classOf[PathCalculator])
}
