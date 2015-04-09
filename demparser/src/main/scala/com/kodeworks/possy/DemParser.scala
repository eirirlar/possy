package com.kodeworks.possy

import java.io.Reader

import scala.util.parsing.combinator.RegexParsers

/*
Source http://nationalmap.gov/standards/pdf/2DEM0198.PDF
 */
object DemParser extends RegexParsers {

  override val skipWhitespace = false

  val name = ".{130}".r ^^ {
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

  def blankN(n: Int): Parser[String] = s"\\s{$n}".r

  val blank4 = blankN(4)

  val blank6 = blankN(6)

  val nameSpace = anyN(6)

  val mapProjectionParameters = anyN(360) //15 fields set to zero when UTM

  val resolution = anyN(12)

  val float: Parser[Float] = " *[-+]?[0-9]*\\.?[0-9]*".r ^^ (_.trim.toFloat)

  def floatN(n: Int) = anyN(n) >> (s => {
    println(s)
    float
  })

  val float12 = floatN(12)

  val float13 = floatN(13)

  val float24 = floatN(24)

  val int: Parser[Int] = " *\\d*".r ^^ (_.trim.toInt)

  def intN(n: Int): Parser[Int] = anyN(n) >> (s => {
    println("int " + s)
    int
  })

  val int3 = intN(3)

  val int4 = intN(4)

  val int5 = intN(5)

  val int6 = intN(6)

  val int8 = intN(8)

  val int9 = intN(9)

  val recordTypeA =
    name ~ int6 ~ nameSpace ~ int6 ~ int6 ~ int6 ~ int6 ~ mapProjectionParameters ~ int6 ~ int6 ~ int6 ~
      float24 ~ float24 ~ float24 ~ float24 ~ float24 ~ float24 ~ float24 ~ float24 ~ float24 ~ float24 ~ float24 ~
      int6 ~ float12 ~ float12 ~ float12 ~ int6 ~ int6 ~ anyN(160) ^^ {
      case name ~ _ ~ _ ~ demLevel ~ elevationPattern ~ planimetricReferenceSystem ~ zone ~ mapProjectionParameters ~ unitOfResolutionGroundGrid ~ unitOfResolutionElevation ~ numberOfSidesInPolygon ~
        eastingOfSW ~ northingOfSW ~ eastingOfNW ~ northingOfNW ~ eastingOfNE ~ northingOfNE ~ eastingOfSE ~ northingOfSE ~ minElevation ~ maxElevation ~ angle ~
        accuracyCode ~ resolutionX ~ resolutionY ~ resolutionZ ~ numberOfRows ~ numberOfColumns ~ _ =>
        RecordTypeA(name, demLevel, elevationPattern, planimetricReferenceSystem, zone, mapProjectionParameters, unitOfResolutionGroundGrid, unitOfResolutionElevation, numberOfSidesInPolygon,
          eastingOfSW, northingOfSW, eastingOfNW, northingOfNW, eastingOfNE, northingOfNE, eastingOfSE, northingOfSE, minElevation, maxElevation, angle,
          accuracyCode, resolutionX, resolutionY, resolutionZ, numberOfRows, numberOfColumns)
    }

  val elevationOpt: DemParser.Parser[Option[Int]] =
    blank6 ^^ { _ => None } | int6 ^^ {
      Some(_)
    }

  def elevationOptsN(n: Int): DemParser.Parser[List[Int]] = repN(n, elevationOpt) ^^ (_.flatten)

  val recordTypeBHead: Parser[RecordTypeB] =
    int6 ~ int6 ~ int6 ~ int6 ~ float24 ~ float24 ~ float24 ~ float24 ~ float24 ~ elevationOptsN(146) ~ blank4 ^^ {
      case rowIdent ~ columnIdent ~ numMElevations ~ numNElevations ~ firstElevationX ~ firstElevationY ~ elevationOfLocalDatum ~ minElevation ~ maxElevation ~ blocks ~ _ =>
        RecordTypeB(rowIdent, columnIdent, numMElevations, numNElevations, firstElevationX, firstElevationY, elevationOfLocalDatum, minElevation, maxElevation, blocks)
    }

  val recordTypeB2: Parser[List[Int]] = elevationOptsN(170) ~ blank4 ^^ {
    case blocks ~ _ => blocks.toList
  }

  val recordTypeBTail: Parser[List[Int]] = not(recordTypeA | recordTypeBHead) ~> opt(recordTypeB2 ~ recordTypeBTail) ^^ {
    case Some(recordTypeB2 ~ recordTypeBTail) => recordTypeB2 ++ recordTypeBTail
    case None => Nil
  }

  val recordTypeB: Parser[RecordTypeB] = recordTypeBHead ~ recordTypeBTail ^^ {
    case head ~ tail => head.copy(elevations = head.elevations ++ tail)
  }

  val dem =
    recordTypeA ~
      rep(recordTypeB) ^^ {
      case typeA ~ typeBs =>
        Dem(typeA, typeBs.toList)
    }


  def parseDem(demString: String): Dem = {
    val p = parse(dem, demString)
    p.get
  }
}

