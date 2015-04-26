package com.kodeworks.possy

import akka.actor.Actor
import com.kodeworks.possy.Model._
import org.slf4j.LoggerFactory
import PathCalculator._
import Boot.possy

class PathCalculator extends Actor {
  var path = List[(Float, Float)]()
  var dem: Dem = null

  override def receive = {
    case CalcPath(l@LatLng(lat, lng)) if path.isEmpty => {
      possy ! GetClosestDem(l)
      calcPath(lat, lng)
    }
    case CalcPath(LatLng(lat, lng)) => {
      calcPath(lat, lng)
    }
    case ResetCalc => {
      log.debug("reset calc")
      path = Nil
      dem = null
      sender ! Ok
    }
    case Some(d: Dem) => this.dem = d
  }

  def calcPath(lat: Float, lng: Float): Unit = {
    log.debug("lat lng")
    path = (lat, lng) :: path
    sender ! path.map(ll => (ll._1 +.02f * math.random.toFloat - .01f) -> (ll._2 +.02f * math.random.toFloat - .01f))
  }
}

object PathCalculator {
  val log = LoggerFactory.getLogger(classOf[PathCalculator])
}
