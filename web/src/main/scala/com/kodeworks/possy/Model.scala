package com.kodeworks.possy

import argonaut.Argonaut._

object Model {

  case class LatLng(
                     lat: Float,
                     lng: Float
                     )

  case object ResetCalc

  case object Ok

  case class GetClosestDem(latLng: LatLng)

  case class CalcPath(latLng: LatLng)

  case class GetElevation(latLng: LatLng)

  implicit def LatLngCodec =
    casecodec2(LatLng.apply, LatLng.unapply)("lat", "lng")
}
