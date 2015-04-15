package com.kodeworks.possy

import scala.util.parsing.combinator.RegexParsers

/*
Source http://nationalmap.gov/standards/pdf/2DEM0198.PDF
 */
object DemParser extends RegexParsers {
  val typeBHeadMaxElevs = 146
  val typeBTailMaxElevs = 170

  override val skipWhitespace = false

  def name = ".{130}".r ^^ {
    _.trim
  }

  def any: Parser[String] = ".*".r

  def anyN(n: Int): Parser[String] = s".{$n}".r

  def any3 = anyN(3)

  def any4 = anyN(4)

  def any6 = anyN(6)

  def any12 = anyN(12)

  def any23 = anyN(23)

  def any132 = anyN(132)

  def blankN(n: Int): Parser[String] = s"\\s{$n}".r

  def blank4 = blankN(4)

  def blank6 = blankN(6)

  def blank8 = blankN(8)

  def mapProjectionParameters = anyN(360) //15 fields set to zero when UTM

  def resolution = anyN(12)

  def strictFloatStringN(n: Int): Parser[String] = {
    val n1 = math.max(1, n - 1)
    val n2 = math.max(1, n - 2)
    s"^(-\\d{$n1})".r |
      s"^(\\d{$n})".r |
      s"^(?=\\d{1,$n2}\\.\\d)[\\d\\.]{$n}".r |
      s"^(?=-\\d{1,$n2}\\.\\d)-[\\d\\.]{$n1}".r
  }

  def strictFloatN(n: Int): Parser[Float] = strictFloatStringN(n) ^^ (_.toFloat)

  def strictFloatN(nBlank: Int, nFloat: Int): Parser[Float] = s" {$nBlank}".r ~> strictFloatN(nFloat)

  def floatN(n: Int) = guard(anyN(n)) >> (f => {
    val nFloat = f.trim.length
    if (0 == nFloat) failure("all blank")
    else strictFloatN(n - nFloat, nFloat)
  })

  def float12 = floatN(12)

  def float13 = floatN(13)

  def float24 = floatN(24)

  def strictShortStringN(n: Int): Parser[String] = s"-\\d{${math.max(1, n - 1)}}|\\d{$n}".r

  def strictShortN(n: Int): Parser[Short] = strictShortStringN(n) ^^ (_.toShort)

  def strictShortN(nBlank: Int, nShort: Int): Parser[Short] = s" {$nBlank}".r ~> strictShortN(nShort)

  def shortN(n: Int): Parser[Short] = guard(anyN(n)) >> (i => {
    val nDigit: Int = i.trim.length
    if (0 == nDigit) failure("all blank")
    else strictShortN(n - nDigit, nDigit)
  })

  def short6 = shortN(6)

  def strictIntStringN(n: Int): Parser[String] = s"-\\d{${math.max(1, n - 1)}}|\\d{$n}".r

  def strictIntN(n: Int): Parser[Int] = strictIntStringN(n) ^^ (_.toInt)

  def strictIntN(nBlank: Int, nInt: Int): Parser[Int] = s" {$nBlank}".r ~> strictIntN(nInt)

  def intN(n: Int): Parser[Int] = guard(anyN(n)) >> (i => {
    val nDigit: Int = i.trim.length
    if (0 == nDigit) failure("all blank")
    else strictIntN(n - nDigit, nDigit)
  })

  def int3 = intN(3)

  def int4 = intN(4)

  def int5 = intN(5)

  def int6 = intN(6)

  def int8 = intN(8)

  def int9 = intN(9)

  def zone = int6

  def recordTypeA =
    name ~ int6 ~ blank8 ~ int6 ~ int6 ~ int6 ~ zone ~ mapProjectionParameters ~ int6 ~ int6 ~ int6 ~
      float24 ~ float24 ~ float24 ~ float24 ~ float24 ~ float24 ~ float24 ~ float24 ~ float24 ~ float24 ~ float24 ~
      int6 ~ float12 ~ float12 ~ float12 ~ int6 ~ int6 ~ anyN(160) ^^ {
      case name ~ _ ~ _ ~ demLevel ~ elevationPattern ~ planimetricReferenceSystem ~ zone ~ mapProjectionParameters ~ unitOfResolutionGroundGrid ~ unitOfResolutionElevation ~ numberOfSidesInPolygon ~
        eastingOfSW ~ northingOfSW ~ eastingOfNW ~ northingOfNW ~ eastingOfNE ~ northingOfNE ~ eastingOfSE ~ northingOfSE ~ minElevation ~ maxElevation ~ angle ~
        accuracyCode ~ resolutionX ~ resolutionY ~ resolutionZ ~ numberOfRows ~ numberOfColumns ~ _ =>
        RecordTypeA(name, demLevel, elevationPattern, planimetricReferenceSystem, zone, mapProjectionParameters, unitOfResolutionGroundGrid, unitOfResolutionElevation, numberOfSidesInPolygon,
          eastingOfSW, northingOfSW, eastingOfNW, northingOfNW, eastingOfNE, northingOfNE, eastingOfSE, northingOfSE, minElevation, maxElevation, angle,
          accuracyCode, resolutionX, resolutionY, resolutionZ, numberOfRows, numberOfColumns)
    }

  def elevationOpt: DemParser.Parser[Option[Short]] =
    blank6 ^^ { _ => None } | short6 ^^ {
      Some(_)
    }

  def elevationOptsN(n: Int): DemParser.Parser[List[Short]] = repN(n, elevationOpt) ^^ (_.flatten)

  def recordTypeBHead: Parser[RecordTypeBHead] =
    int6 ~ int6 ~ int6 ~ int6 ~ float24 ~ float24 ~ float24 ~ float24 ~ float24 ~ elevationOptsN(typeBHeadMaxElevs) ~ blank4 ^^ {
      case rowIdent ~ columnIdent ~ numMElevations ~ numNElevations ~ firstElevationX ~ firstElevationY ~ elevationOfLocalDatum ~ minElevation ~ maxElevation ~ blocks ~ _ =>
        RecordTypeBHead(rowIdent, columnIdent, numMElevations, numNElevations, firstElevationX, firstElevationY, elevationOfLocalDatum, minElevation, maxElevation, blocks)
    }

  def recordTypeBTailElevations: Parser[List[Short]] = elevationOptsN(typeBTailMaxElevs) ~ blank4 ^^ {
    case blocks ~ _ => blocks.toList
  }

  def recordTypeBTail: Parser[RecordTypeBTail] = recordTypeBTailElevations ^^ (RecordTypeBTail(_))

  def recordTypeBTailBak: Parser[List[Short]] = recordTypeBTailElevations ~ opt(recordTypeBTailBak) ^^ {
    case recordTypeB2 ~ Some(recordTypeBTail) => recordTypeB2 ++ recordTypeBTail
    case recordTypeB2 ~ _ => recordTypeB2
  }

  def recordTypeB: Parser[RecordTypeBHead] = recordTypeBHead ~ opt(recordTypeBTailBak) ^^ {
    case head ~ Some(tail) => head.copy(elevations = head.elevations ++ tail)
    case head ~ _ => head
  }

  def record: Parser[Record] = recordTypeBTail | recordTypeBHead | recordTypeA

  def dem =
    recordTypeA ~
      rep(recordTypeB) ^^ {
      case typeA ~ typeBs =>
        Dem(typeA, typeBs.toList)
    }

  def parseDem(demString: String): Dem = {
    parse(dem, demString).get
  }
}

