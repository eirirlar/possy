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
  implicit val timeout = Timeout(5 seconds)

  var path = List[(Float, Float)]()
  var dem: SimpleDem = null

  override def receive = {
    case CalcPath(ll@LatLng(lat, lng)) => checkNoDem(ll) onSuccess {
      case sender => sender ! calcPath(lat, lng)
    }
    case GetElevation(l@LatLng(lat, lng)) => checkNoDem(l) onSuccess {
      case sender => sender ! elevation(lat, lng)
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

  def calcPath(lat: Float, lng: Float): List[(Float, Float)] = {
    log.debug("lat lng")
    path = (lat, lng) :: path
    val elevs = elevations
    path.map(ll => (ll._1 +.02f * math.random.toFloat - .01f) -> (ll._2 +.02f * math.random.toFloat - .01f))
  }

  def elevation(lat: Float, lng: Float): Short = {
    val u = Geokonvert.transformToUTM(lat, lng, DatumProvider.WGS84, true)
    val (n, e) = dem.snapToGrid(u.N.toInt, u.E.toInt)
    dem.grid(n, e)
  }

  def elevations =
    path.map(ll => elevation(ll._1, ll._2))
}

object PathCalculator {
  val log = LoggerFactory.getLogger(classOf[PathCalculator])
}
