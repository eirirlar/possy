package com.kodeworks.possy

import argonaut.Argonaut._

object Model {
  case class LatLng(
                     lat: Float,
                     lng: Float
                     )

  implicit def LatLngCodec =
    casecodec2(LatLng.apply, LatLng.unapply)("lat", "lng")
}
