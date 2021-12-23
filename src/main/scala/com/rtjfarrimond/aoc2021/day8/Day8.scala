package com.rtjfarrimond.aoc2021.day8

import scala.io.Source

@main
def runDay8(): Unit =
  println(s"Part one: ${Day8.part1}")
  println(s"Part two: ${Day8.part2}")


case class OutputValue(values: List[String])

object Day8:

  val input: List[String] = Source.fromResource("Day8.txt")
    .getLines
    .toList

  val outputValues: List[OutputValue] =
      input.map { line =>
        line
          .split("\\|")
          .toList(1)
      }
      .map(_.trim.split(' ').toList)
      .map(OutputValue.apply)

  def is1(input: String): Boolean =
    input.length == 2

  def is4(input: String): Boolean =
    input.length == 4

  def is7(input: String): Boolean =
    input.length == 3

  def is8(input: String): Boolean =
    input.length == 7

  def part1: Int =
    outputValues.map { outputValue =>
      outputValue
        .values
        .map(s => is1(s) || is4(s) || is7(s) || is8(s))
        .filter(_ == true)
        .length
    }.sum

  def part2: Int =
    42
