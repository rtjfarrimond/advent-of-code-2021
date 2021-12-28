package com.rtjfarrimond.aoc2021.day9


sealed trait HeightMap {

  def locations: Map[Coordinate, Int]
  def lowPoints: Map[Coordinate, Int]
  def riskLevels: Map[Coordinate, Int]

  def isLowPoint(coordinate: Coordinate): Boolean
  def heightAt(coordinate: Coordinate): Option[Int]
  def heightAbove(coordinate: Coordinate): Option[Int]
  def heightBelow(coordinate: Coordinate): Option[Int]
  def heightLeftOf(coordinate: Coordinate): Option[Int]
  def heightRightOf(coordinate: Coordinate): Option[Int]

}

object HeightMap {

  def apply(input: List[Int], maxX: Int): HeightMap = new HeightMap {

    override val locations: Map[Coordinate, Int] =
      input.zipWithIndex.map { (height, index) =>
        val y = index / maxX
        val x = index % maxX
        Coordinate(x, y) -> height
      }.toMap

    override val riskLevels: Map[Coordinate, Int] =
      locations
        .map { (coord, height) =>
          coord -> (height + 1)
        }

    override def heightAt(coordinate: Coordinate): Option[Int] =
      locations.get(coordinate)

    override def heightAbove(coordinate: Coordinate): Option[Int] =
      if (coordinate.y == 0) None
      else locations.get(coordinate.above)

    private val maxY = locations.keys.map(_.y).max
    override def heightBelow(coordinate: Coordinate): Option[Int] =
      if (coordinate.y == maxY) None
      else locations.get(coordinate.below)

    override def heightLeftOf(coordinate: Coordinate): Option[Int] =
      if (coordinate.x == 0) None
      else locations.get(coordinate.leftOf)

    override def heightRightOf(coordinate: Coordinate): Option[Int] =
      if (coordinate.x == maxX) None
      else locations.get(coordinate.rightOf)

    override def isLowPoint(coordinate: Coordinate): Boolean =
      val heightAtCoordinate = locations(coordinate)
      val neighbourOptions =
        List(
          heightAbove(coordinate),
          heightRightOf(coordinate),
          heightBelow(coordinate),
          heightLeftOf(coordinate))
      neighbourOptions
        .collect {
          case Some(neighbourHeight) =>
            neighbourHeight > heightAtCoordinate
        }
        .forall(_ == true)

    override def lowPoints: Map[Coordinate, Int] =
      locations
        .filter((coord, _) => isLowPoint(coord))

  }

}
