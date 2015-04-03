package com.kodeworks.possy

case class Dem(
                name: String,
                utm: Int,
                unitOfResolutionGroundGrid: Int,
                unitOfResolutionElevation: Int,
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