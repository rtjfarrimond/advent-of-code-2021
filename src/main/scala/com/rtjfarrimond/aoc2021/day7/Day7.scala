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
  type CostFn = (Int, Int) => FuelCost

  def getFuelCost: CostFn = (from, to) =>
    scala.math.abs(from - to)

  def getTriangularFuelCost: CostFn = (from, to) =>
    val n = scala.math.abs(from - to)
    n * (n + 1) / 2

  def part1: Int =
    solve(getFuelCost)

  def part2: Int =
    solve(getTriangularFuelCost)

  def solve(fn: CostFn): FuelCost =
    val minPosition = input.min
    val maxPosition = input.max
    val range = (minPosition to maxPosition)
    val matrix =
      range.map { targetPosition =>
        input.map { crabPosition =>
          fn(crabPosition, targetPosition)
        }
      }
    matrix.map(_.sum).min
