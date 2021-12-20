package com.rtjfarrimond.aoc2021.day6

import scala.io.Source

@main
def runDay6(): Unit =
  println(s"Part one: ${Day6.part1(80)}")
  println(s"Part two: ${Day6.part2}")


object Day6 {

  val input: List[LanternFish] = Source.fromResource("Day6.txt")
    .getLines
    .toList
    .head
    .split(',')
    .toList
    .map(_.toInt)
    .map(LanternFish.adult)

  def part1(nDays: Int): Long =
    val days = (0 until nDays).toList
    val schoolAfterNDays = days.foldLeft(School(input)) {
      case (school, day) =>
        school.ageOneDay
    }
    schoolAfterNDays.fish.values.sum

  def part2: Long =
    part1(256)

}
