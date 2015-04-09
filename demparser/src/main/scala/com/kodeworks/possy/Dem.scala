package com.kodeworks.possy

case class Dem(
                typeA: RecordTypeA,
                typeBs: List[RecordTypeBHead]
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
                        elevations: List[Short] //146 elems in first B record, 170 in each next typeB
                        ) extends Record

case class RecordTypeBTail(
                          elevations: List[Short]
                            ) extends Record