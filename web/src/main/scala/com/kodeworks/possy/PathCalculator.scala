package com.kodeworks.possy

import akka.actor.Actor
import com.kodeworks.possy.Model.{Ok, ResetCalc, LatLng}
import org.slf4j.LoggerFactory
import PathCalculator._

class PathCalculator extends Actor {
  var path = List[(Float, Float)]()

  override def receive = {
    case LatLng(lat, lng) => {
      log.debug("lat lng")
      path = (lat, lng) :: path
      sender ! path.map(ll => (ll._1 + math.random.toFloat - .5f) -> (ll._2 + math.random.toFloat - .5f))
    }
    case ResetCalc => {
      log.debug("reset calc")
      path = Nil
      sender ! Ok
    }
  }
}

object PathCalculator {
  val log = LoggerFactory.getLogger(classOf[PathCalculator])
}