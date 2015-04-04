package com.kodeworks.possy

case class Dem(
                header: DemHeader,
                block: Block
                )

case class DemHeader(
                      name: String,
                    demLevel:Int, //1=DEM-1
                      elevationPattern:Int, //1=regular
                      planimetricReferenceSystem:Int, //1=UTM
                      zone: Int,
                      mapProjectionParameters:String,
                      unitOfResolutionGroundGrid: String, //2=meter
                      unitOfResolutionElevation: String, //2=meter
                      numberOfSidesInPolygon:String, // must be 4
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
                      resolutionPerGridCellEW: Float,
                      resolutionPerGridCellNS: Float,
                      multiplier: Float,
                      numberOfColumns: Int
                      )

case class Block(
                  rowNumber: Int,
                  columnNumber: Int,
                  numberOfColumns: Int,
                  block: String
                  )