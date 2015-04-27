package com.kodeworks.possy

import breeze.linalg.DenseMatrix

import scala.collection.immutable.VectorBuilder
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


case class Dem(
                typeA: RecordTypeA,
                typeBs: Vector[RecordTypeBHead]
                )

sealed trait Record

case class RecordTypeA(
                        name: String,
                        demLevel: Int, //1=DEM-1
                        elevationPattern: Int, //1=regular
                        planimetricReferenceSystem: Int, //1=UTM
                        zone: Int,
                        mapProjectionParameters: String,
                        unitOfResolutionGroundGrid: Int, //2=meter
                        unitOfResolutionElevation: Int, //2=meter
                        numberOfSidesInPolygon: Int, // must be 4
                        eastingOfSW: Float,
                        northingOfSW: Float,
                        eastingOfNW: Float,
                        northingOfNW: Float,
                        eastingOfNE: Float,
                        northingOfNE: Float,
                        eastingOfSE: Float,
                        northingOfSE: Float,
                        minElevation: Float,
                        maxElevation: Float,
                        angle: Float, //CCW angle in radians from ground planimetric reference to local reference
                        accuracyCode: Int, //0=unknown
                        resolutionX: Float,
                        resolutionY: Float,
                        resolutionZ: Float,
                        numberOfRows: Int,
                        numberOfColumns: Int
                        ) extends Record


case class RecordTypeBHead(
                            rowNumber: Int,
                            columnNumber: Int,
                            numMElevations: Int,
                            numNElevations: Int,
                            firstElevationX: Float,
                            firstElevationY: Float,
                            elevationOfLocalDatum: Float,
                            minElevation: Float,
                            maxElevation: Float,
                            elevations: Vector[Short] //146 elems in first B record, 170 in each next typeB
                            ) extends Record

case class RecordTypeBTail(
                            elevations: Vector[Short]
                            ) extends Record

class SimpleDem(
                 val name: String,
                 val northingOfNE: Int,
                 val eastingOfNE: Int,
                 val northingOfSW: Int,
                 val eastingOfSW: Int,
                 val resolutionX: Int,
                 val resolutionY: Int,
                 val grid: DenseMatrix[Short]) {
  private def snapTo(snappable: Int, resolution: Int): Int = {
    math.round(snappable.toFloat / resolution.toFloat)
  }

  def snapToGrid(northing: Int, easting: Int): (Int, Int) = {
    snapTo(northing - northingOfSW, resolutionY) -> snapTo(easting - eastingOfSW, resolutionX)
  }
}

//usage: typeA, typeBHead, typeBTail, typeBTail, typeBHead, typeBTail, typeBTail etc. change of this order is not supported
class DemBuilder {
  var typeA: RecordTypeA = null
  var nTypeBs: Int = -1
  var nTypeBTails: Int = -1
  var typeBHead: RecordTypeBHead = null
  var typeBTails: VectorBuilder[RecordTypeBTail] = null
  var typeBs: VectorBuilder[RecordTypeBHead] = null

  def apply(typeA: RecordTypeA) {
    this.typeA = typeA
    this.nTypeBs = typeA.numberOfColumns
    this.typeBs = new VectorBuilder()
  }

  def apply(typeBHead: RecordTypeBHead) {
    checkNTypeBTails(typeBHead)
    checkBuildTypeB()
    this.typeBHead = typeBHead
  }

  def checkNTypeBTails(typeBHead: RecordTypeBHead): Unit = {
    if (-1 == this.nTypeBTails) {
      this.nTypeBTails = if (typeBHead.numMElevations < DemParser.typeBHeadMaxElevs) 0
      else {
        val d = typeBHead.numMElevations - DemParser.typeBHeadMaxElevs
        d / DemParser.typeBTailMaxElevs + (if (d % DemParser.typeBTailMaxElevs == 0) 0 else 1)
      }
      typeBTails = new VectorBuilder()
    }
  }

  def apply(typeBTail: RecordTypeBTail) {
    this.typeBTails += typeBTail
  }

  def apply(record: Record): DemBuilder = {
    record match {
      case a: RecordTypeA => apply(a)
      case bh: RecordTypeBHead => apply(bh)
      case bt: RecordTypeBTail => apply(bt)
    }
    this
  }

  def checkBuildTypeB() {
    if (null != typeBHead) {
      typeBs += typeBHead.copy(elevations = typeBHead.elevations ++ typeBTails.result().flatMap(_.elevations))
      typeBHead = null
    }
    typeBTails = new VectorBuilder()
  }

  def build() = {
    checkBuildTypeB
    Dem(typeA, typeBs.result)
  }
}

class SimpleDemBuilder {
  var typeA: RecordTypeA = null
  var cols: Int = -1
  val grid: mutable.ArrayBuilder[Short] = mutable.ArrayBuilder.make()

  def apply(typeA: RecordTypeA) {
    this.typeA = typeA
  }

  def apply(typeBHead: RecordTypeBHead) {
    cols = typeBHead.numMElevations
    grid ++= typeBHead.elevations
  }

  def apply(typeBTail: RecordTypeBTail) {
    this.grid ++= typeBTail.elevations
  }

  def apply(record: Record): SimpleDemBuilder = {
    record match {
      case a: RecordTypeA => apply(a)
      case bh: RecordTypeBHead => apply(bh)
      case bt: RecordTypeBTail => apply(bt)
    }
    this
  }

  def build() = {
    new SimpleDem(
      typeA.name,
      typeA.northingOfNE.toInt,
      typeA.eastingOfNE.toInt,
      typeA.northingOfSW.toInt,
      typeA.eastingOfSW.toInt,
      typeA.resolutionX.toInt,
      typeA.resolutionY.toInt,
      DenseMatrix.create(typeA.numberOfColumns, cols, grid.result))
  }
}

