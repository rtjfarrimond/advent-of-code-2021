package com.rtjfarrimond.aoc2021.day5

import scala.io.Source

@main
def runDay5(): Unit =
  println(s"Part one: ${Day5.part1}")
  // println(s"Part two: ${Day5.part2}")


object Day5 {

  val input: List[Vector] = Source.fromResource("Day5.txt")
    .getLines
    .toList
    .map(_.replace(" -> ", ","))
    .map(_.split(',').toList.map(_.toInt))
    .map { line =>
      val from = Coordinate(line(0), line(1)) 
      val to = Coordinate(line(2), line(3)) 
      Vector(from, to)
    }

  def part1: Int =
    val filtered = input.filter(!_.isDiagonal)
    val occupied = getOccupied(filtered)
    val overlaps = getOverlaps(occupied)
    val overlapsGreaterThanOne = overlaps.collect {
      case (coordinate, overlapSize) if overlapSize > 1 =>
        (coordinate, overlapSize)
    }
    overlapsGreaterThanOne.keySet.size

  def getOccupied(vectors: List[Vector]): List[Coordinate] =
    vectors.flatMap { vector =>
      if (vector.from.y == vector.to.y) {
        val y = vector.to.y
        val minX = vector.from.x min vector.to.x
        val maxX = vector.from.x max vector.to.x
        (minX to maxX)
          .toList
          .map(x => Coordinate(x, y))
      } else {
        val x = vector.to.x
        val minY = vector.from.y min vector.to.y
        val maxY = vector.from.y max vector.to.y
        (minY to maxY).toList
          .toList
          .map(y => Coordinate(x, y))
      }
    }

  def getOverlaps(occupied: List[Coordinate]): Map[Coordinate, Int] =
    occupied.groupMapReduce(identity)(_ => 1)(_ + _)

  def part2: Int =
    -42

}
