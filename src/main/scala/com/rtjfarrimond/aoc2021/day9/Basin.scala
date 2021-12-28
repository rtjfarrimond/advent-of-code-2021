package com.rtjfarrimond.aoc2021.day9

case class Basin(lowPoint: Coordinate, locations: Set[Coordinate]) {
  val area: Int = locations.size
}


object Basin {

  def explore(heightMap: HeightMap): Set[Basin] =
    heightMap
      .lowPoints
      .keySet
      .map { lowPointCoordinate =>
        val exploration = exploreOneLowPoint(heightMap, lowPointCoordinate)
        Basin(lowPointCoordinate, exploration.locations)
      }

  private case class Exploration(
    toExplore: Set[Coordinate],
    locations: Set[Coordinate],
    boundary: Set[Coordinate]
  )

  private object Exploration {

    def initial(position: Coordinate): Exploration =
      Exploration(Set(position), Set.empty, Set.empty)

    extension(exp1: Exploration)

      def addToExplore(coordinate: Coordinate): Exploration =
        val newToExplore = exp1.toExplore + coordinate
        Exploration(newToExplore, exp1.locations, exp1.boundary)

      def removeToExplore(coordinate: Coordinate): Exploration =
        val newToExplore = exp1.toExplore - coordinate
        Exploration(newToExplore, exp1.locations, exp1.boundary)

      def addLocation(coordinate: Coordinate): Exploration =
        val newLocations = exp1.locations + coordinate
        Exploration(exp1.toExplore, newLocations, exp1.boundary)

      def addBoundary(coordinate: Coordinate): Exploration =
        val newBoundary = exp1.boundary + coordinate
        Exploration(exp1.toExplore, exp1.locations, newBoundary)

  }

  import Exploration._
  private def exploreOneLowPoint(
    heightMap: HeightMap,
    lowPoint: Coordinate
  ): Exploration = {

    def loop(heightMap: HeightMap, acc: Exploration): Exploration =
      if (acc.toExplore.isEmpty) acc
      else {
        val currentPosition = acc.toExplore.head

        val neighbours =
          currentPosition
            .neighbours
            .filterNot { nbr =>
              (acc.boundary ++ acc.locations).contains(nbr)
            }
            .map { nbr =>
              heightMap
                .locations
                .get(nbr)
                .map(height => nbr -> height)
            }

        val boundaryNeighbours =
          neighbours
            .collect {
              case Some((nbr, height)) if height == 9 =>
                nbr
            }
            .toSet

        val toExploreLater =
          neighbours
            .collect {
              case Some((nbr, height)) if height < 9 =>
                nbr
            }
            .toSet

        val explorationWithUpdatedToExploreLater =
          toExploreLater.foldLeft(acc) { (acc, nextCoord) =>
            acc.addToExplore(nextCoord)
          }
          .removeToExplore(currentPosition)


        val explorationWithUpdatedBoundary =
          boundaryNeighbours.foldLeft(explorationWithUpdatedToExploreLater) { (acc, nextCoord) =>
            acc.addBoundary(nextCoord)
          }

        val nextAcc = explorationWithUpdatedBoundary.addLocation(currentPosition)
        loop(heightMap, nextAcc)

      }

    loop(heightMap, Exploration.initial(lowPoint))
  }

}
