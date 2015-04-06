package com.kodeworks.possy

import java.io.Reader

import scala.util.parsing.combinator.RegexParsers

/*
Source http://nationalmap.gov/standards/pdf/2DEM0198.PDF
 */
object DemParser extends RegexParsers {

  override val skipWhitespace = false

  val name = ".{135}".r ^^ {
    _.trim
  }

  val any: Parser[String] = ".*".r

  def anyN(n: Int): Parser[String] = s".{$n}".r

  val any3 = anyN(3)

  val any4 = anyN(4)

  val any6 = anyN(6)

  val any12 = anyN(12)

  val any23 = anyN(23)

  val any132 = anyN(132)

  def blankN(n:Int):Parser[String] = s"\\s{$n}".r

  val blank4 = blankN(4)

  val nameSpace = anyN(9)

  val mapProjectionParameters = anyN(360) //15 fields set to zero when UTM

  val resolution = anyN(12)

  def floatN(n: Int) = anyN(n) ^^ (_.toFloat)

  val float12 = floatN(12)

  val float13 = floatN(13)

  val float24 = floatN(24)

  def intN(n: Int): Parser[Int] = s"[\\d ]{$n}".r ^^ (_.trim.toInt)

  val int3 = intN(3)

  val int4 = intN(4)

  val int5 = intN(5)

  val int6 = intN(6)

  val int8 = intN(8)

  val int9 = intN(9)

  val zone = int6

  val recordTypeA =
    name ~ nameSpace ~ int6 ~ int6 ~ int6 ~ zone ~ mapProjectionParameters ~ int6 ~ int6 ~ int6 ~
      float24 ~ float24 ~ float24 ~ float24 ~ float24 ~ float24 ~ float24 ~ float24 ~ float24 ~ float24 ~ float24 ~
      int6 ~ float12 ~ float12 ~ float12 ~ int6 ~ int6 ~ anyN(160) ^^ {
      case name ~ _ ~ demLevel ~ elevationPattern ~ planimetricReferenceSystem ~ zone ~ mapProjectionParameters ~ unitOfResolutionGroundGrid ~ unitOfResolutionElevation ~ numberOfSidesInPolygon ~
        eastingOfSW ~ northingOfSW ~ eastingOfNW ~ northingOfNW ~ eastingOfNE ~ northingOfNE ~ eastingOfSE ~ northingOfSE ~ minElevation ~ maxElevation ~ angle ~
        accuracyCode ~ resolutionX ~ resolutionY ~ resolutionZ ~ numberOfRows ~ numberOfColumns ~ _ =>
        RecordTypeA(name, demLevel, elevationPattern, planimetricReferenceSystem, zone, mapProjectionParameters, unitOfResolutionGroundGrid, unitOfResolutionElevation, numberOfSidesInPolygon,
          eastingOfSW, northingOfSW, eastingOfNW, northingOfNW, eastingOfNE, northingOfNE, eastingOfSE, northingOfSE, minElevation, maxElevation, angle,
          accuracyCode, resolutionX, resolutionY, resolutionZ, numberOfRows, numberOfColumns)
    }

  val recordTypeB =
    int6 ~ int6 ~ int6 ~ int6 ~ float24 ~ float24 ~ float24 ~ float24 ~ float24 ~ repN(146, int6) ~ blank4 ^^ {
      case rowIdent ~ columnIdent ~ numMElevations ~ numNElevations ~ firstElevationX ~ firstElevationY ~ elevationOfLocalDatum ~ minElevation ~ maxElevation ~ blocks ~ _ =>
        RecordTypeB(rowIdent, columnIdent, numMElevations, numNElevations, firstElevationX, firstElevationY, elevationOfLocalDatum, minElevation, maxElevation, blocks.toList)
    }

  val recordTypeB2 =repN(170, int6) ~ blank4 ^^ {
    case blocks ~ _ => blocks.toList
  }

  val dem =
    recordTypeA ~
      recordTypeB ~
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

//    00    00   0   0 0 0 0 3   0   0 0 0 0 00.00                                                                                                                     1