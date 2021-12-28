package com.rtjfarrimond.aoc2021.day9

case class Coordinate(x: Int, y: Int)

object Coordinate {

  extension(coord: Coordinate)
    def above: Coordinate =
      Coordinate(coord.x, coord.y - 1)

    def below: Coordinate =
      Coordinate(coord.x, coord.y + 1)

    def leftOf: Coordinate =
      Coordinate(coord.x - 1, coord.y)

    def rightOf: Coordinate =
      Coordinate(coord.x + 1, coord.y)

    def neighbours: Set[Coordinate] =
      Set(coord.above, coord.rightOf, coord.below, coord.leftOf)

}

