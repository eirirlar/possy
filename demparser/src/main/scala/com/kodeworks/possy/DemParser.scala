package com.kodeworks.possy

import java.io.Reader

import scala.util.parsing.combinator.RegexParsers

/*
Source http://nationalmap.gov/standards/pdf/2DEM0198.PDF
 */
object DemParser extends RegexParsers {

  val name = ".{135}".r ^^ {
    _.trim
  }

  val any: Parser[String] = ".*".r

  def anyN(n: Int): Parser[String] = s"[.\\s]{$n}".r

  val any3 = anyN(3)

  val any4 = anyN(4)

  val any6 = anyN(6)

  val any12 = anyN(12)

  val any23 = anyN(23)

  val any132 = anyN(132)

  val nameSpace = anyN(10)

  val mapProjectionParameters = anyN(353) //15 fields set to zero when UTM

  val resolution = anyN(12)

  def floatN(n: Int) = anyN(n) ^^ (_.toFloat)

  val float12 = floatN(12)

  val float13 = floatN(13)

  val float23 = floatN(23)

  def intN(n: Int): Parser[Int] = s"[\\d ]{$n}".r ^^ (_.trim.toInt)

  val int3 = intN(3)

  val int4 = intN(4)

  val int5 = intN(5)

  val int6 = intN(6)

  val int8 = intN(8)

  val int9 = intN(9)

  val zone = int6

  val header =
    name ~ nameSpace ~ int6 ~ int6 ~ int5 ~ zone ~ mapProjectionParameters ~ anyN(15) /*~ any6 ~ any6 ~ any6 */ ~ float23 ~ float23 ~ float23 ~ float23 ~
      float23 ~ float23 ~ float23 ~ float23 ~ float23 ~ float23 ~ any23 ~ float12 ~ float12 ~
      float13 ~ any3 ~ int8 ~ any132 ^^ {
      case name ~ _ ~ demLevel ~ elevationPattern ~ planimetricReferenceSystem ~ zone ~ mapProjectionParameters ~ unitOfResolutionGroundGrid /*~ unitOfResolutionElevation ~ numberOfSidesInPolygon*/ ~ eastingOfSW ~ northingOfSW ~ eastingOfNW ~ northingOfNW ~
        eastingOfNE ~ northingOfNE ~ eastingOfSE ~ northingOfSE ~ minElevation ~ maxElevation ~ _ ~ resolutionPerGridCellEW ~ resolutionPerGridCellNS ~
        multiplier ~ _ ~ numberOfColumns ~ _ =>
        DemHeader(name, demLevel, elevationPattern, planimetricReferenceSystem, zone, mapProjectionParameters, unitOfResolutionGroundGrid, "", "", /*unitOfResolutionElevation, numberOfSidesInPolygon, */ eastingOfSW, northingOfSW, eastingOfNW, northingOfNW,
          eastingOfNE, northingOfNE, eastingOfSE, northingOfSE, minElevation, maxElevation, resolutionPerGridCellEW, resolutionPerGridCellNS,
          multiplier, numberOfColumns)
    }

  //row, column, num cols, ? (1),
  val block = int6 ~ int3 ~ int9 ~ any3 ~ anyN(500) ^^ {
    case rowNumber ~ columnNumber ~ numberOfColumns ~ _ ~ any => Block(rowNumber, columnNumber, numberOfColumns, any)
  }


  val dem =
    header ~ //TODO eirirlar start block parsing here
      block ~
      any ^^ {
      case header ~ block ~ _ =>
        Dem(header, block)
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

