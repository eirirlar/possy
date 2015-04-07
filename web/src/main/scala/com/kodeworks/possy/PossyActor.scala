package com.kodeworks.possy

import akka.actor.Actor
import PossyActor._
import argonaut._, Argonaut._

class PossyActor() extends Actor {
  override def receive = {
    case LoadClosestElevationModel(lat, lng) => sender ! ElevationModel(lat + 5f, lng - 5f, lat - 5f, lng +  5f)
  }
}

object PossyActor {
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
