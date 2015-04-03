package com.kodeworks.possy

import java.io.Reader

import scala.util.parsing.combinator.RegexParsers

object DemParser extends RegexParsers {

  val name = ".{135}".r ^^ {
    _.trim
  }

  val any = ".*".r

  def anyN(n: Int): Parser[String] = s".{$n}".r

  val any3 = anyN(3)

  val any4 = anyN(4)

  val any6 = anyN(6)

  val any12 = anyN(12)

  val any23 = anyN(23)

  val any132 = anyN(132)

  val nameSpace = anyN(27)

  val utmSpace = anyN(358)

  val resolution = anyN(12)

  def floatN(n: Int) = anyN(n) ^^ (_.toFloat)

  val float12 = floatN(12)

  val float13 = floatN(13)

  val float23 = floatN(23)

  def intN(n: Int): Parser[Int] = s"[\\d ]{$n}".r ^^ (_.trim.toInt)

  val int3 = intN(3)

  val int4 = intN(4)

  val int6 = intN(6)

  val int8 = intN(8)

  val utm = int6

  val unitOfResolutionGroundGrid = int6

  val unitOfResolutionElevation = int3

  val dem =
    name ~
      nameSpace ~
      utm ~
      utmSpace ~
      unitOfResolutionGroundGrid ~
      unitOfResolutionElevation ~
      any3 ~
      float23 ~
      float23 ~
      float23 ~
      float23 ~
      float23 ~
      float23 ~
      float23 ~
      float23 ~
      float23 ~
      float23 ~
      any23 ~
      float12 ~
      float12 ~
      float13 ~
      any3 ~
      int8 ~
      any132 ~ //TODO eirirlar start block parsing here
      any ^^ {
      case name ~ _ ~ utm ~ _ ~ unitOfResolutionGroundGrid ~ unitOfResolutionElevation ~ _ ~ eastingOfSW ~ northingOfSW ~ eastingOfNW ~ northingOfNW ~
        eastingOfNE ~ northingOfNE ~ eastingOfSE ~ northingOfSE ~ minElevation ~ maxElevation ~ _ ~ resolutionPerGridCellEW ~ resolutionPerGridCellNS ~
        multiplier ~ _ ~ numberOfColumns ~ _ ~ _ =>
        Dem(name, utm, unitOfResolutionGroundGrid, unitOfResolutionElevation, eastingOfSW, northingOfSW, eastingOfNW, northingOfNW,
          eastingOfNE, northingOfNE, eastingOfSE, northingOfSE, minElevation, maxElevation, resolutionPerGridCellEW, resolutionPerGridCellNS, multiplier, numberOfColumns)
    }


  def parseDem(input: Reader): Dem = {
    val parsed: DemParser.ParseResult[Dem] = parse(dem, input)
    parsed match {
      case Success(r, next) =>
      case Failure(msg, next) => {
        println(msg)
      }
    }
    parsed.get
  }
}

