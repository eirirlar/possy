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

  implicit def LatLngCodec =
    casecodec2(LatLng.apply, LatLng.unapply)("lat", "lng")

  implicit def GetClosestDemCodec =
    casecodec1(GetClosestDem.apply, GetClosestDem.unapply)("latLng")

  implicit def CalcPathCodec =
    casecodec1(CalcPath.apply, CalcPath.unapply)("latLng")
}
