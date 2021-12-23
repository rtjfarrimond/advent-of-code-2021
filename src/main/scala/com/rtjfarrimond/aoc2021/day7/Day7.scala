package com.rtjfarrimond.aoc2021.day7

import scala.io.Source


@main
def runDay7(): Unit =
  println(s"Part one: ${Day7.part1}")
  println(s"Part two: ${Day7.part2}")


object Day7:

  val input: List[Int] = Source.fromResource("Day7.txt")
    .getLines
    .toList
    .head
    .split(',')
    .map(_.toInt)
    .toList

  type FuelCost = Int

  def getFuelCost(from: Int, to: Int): FuelCost =
    scala.math.abs(from - to)

  def part1: Int =
    val minPosition = input.min
    val maxPosition = input.max
    val range = (minPosition to maxPosition)
    val matrix = range.map { targetPosition =>
      input.map { crabPosition =>
        getFuelCost(crabPosition, targetPosition)
      }
    }
    matrix.map(_.sum).min

  def part2: Int =
    42
