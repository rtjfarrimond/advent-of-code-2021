package com.rtjfarrimond.aoc2021.day9

import scala.io.Source

@main
def runDay9(): Unit =
  println(s"Part one: ${Day9.part1}")
  println(s"Part two: ${Day9.part2}")


object Day9:

  val input: List[List[Int]] = Source.fromResource("Day9.txt")
    .getLines
    .toList
    .map { line =>
      line
        .map(_.toString.toInt)
        .toList
    }

  def part1: Int =
    val xAxisLength = input.head.length
    val heightMap = HeightMap(input.flatten, xAxisLength)
    heightMap
      .lowPoints
      .map((coord, _) => heightMap.riskLevels(coord))
      .sum

  def part2: Int =
    42
