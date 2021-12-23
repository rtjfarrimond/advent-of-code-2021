package com.rtjfarrimond.aoc2021.day8

import scala.io.Source

@main
def runDay8(): Unit =
  println(s"Part one: ${Day8.part1}")
  println(s"Part two: ${Day8.part2}")


case class SignalPattern(pattern: List[String])
case class OutputValue(values: List[String])
case class Entry(signalPattern: SignalPattern, outputValue: OutputValue)

object Day8:

  val input: List[Entry] = Source.fromResource("Day8.txt")
    .getLines
    .toList
    .map { line =>
      val split = line.split("\\|").toList.map(_.trim)
      val signalPattern = SignalPattern(split(0).split(' ').toList)
      val outputValue = OutputValue(split(1).split(' ').toList)
      Entry(signalPattern, outputValue)
    }

  def is1(input: String): Boolean =
    input.length == 2

  def is4(input: String): Boolean =
    input.length == 4

  def is7(input: String): Boolean =
    input.length == 3

  def is8(input: String): Boolean =
    input.length == 7

  def canBeDeterminedByLength(s: String): Boolean =
    is1(s) || is4(s) || is7(s) || is8(s)

  def part1: Int =
    val outputValues = input.map(_.outputValue)
    outputValues.map { outputValue =>
      outputValue
        .values
        .map(canBeDeterminedByLength)
        .filter(_ == true)
        .length
    }.sum

  def is2Or3Or5(input: String): Boolean =
    input.length == 5

  def is6Or9Or0(input: String): Boolean =
    input.length == 6

  def determineByLength(code: String): Int =
    code.length match {
      case 2 => 1
      case 4 => 4
      case 3 => 7
      case 7 => 8
    }

  def determineIf6Or9Or0(code: String, one: Set[Char], four: Set[Char]): Int =
    if ((code.toSet intersect four.toSet) == four.toSet) 9
    else if ((code.toSet intersect one.toSet) == one.toSet) 0
    else 6

  def determineIf2Or3Or5(code: String, one: Set[Char], nine: Set[Char]): Int =
    if ((code.toSet intersect one.toSet) == one.toSet) 3
    else if ((code.toSet intersect nine.toSet) == code.toSet) 5
    else 2

  def buildCodec(signalPattern: SignalPattern): Map[Set[Char], Int] =
    val partitioned = signalPattern.pattern.partition(canBeDeterminedByLength)
    val toBeDeterminedByLength = partitioned._1
    val decodedByLength =
      toBeDeterminedByLength
        .map(s => determineByLength(s) -> s.toSet)
        .toMap

    val cannotBeDeterminedByLength = partitioned._2
    val furtherPartitioned = cannotBeDeterminedByLength.partition(is6Or9Or0)
    val sixAndNineAndZeroCodes = furtherPartitioned._1
    val sixAndNineAndZero =
      sixAndNineAndZeroCodes
        .map(s => determineIf6Or9Or0(s, decodedByLength(1), decodedByLength(4)) -> s.toSet)
        .toMap

    val twoAndThreeAndFiveCodes = furtherPartitioned._2
    val twoAndThreeAndFive =
      twoAndThreeAndFiveCodes
        .map(s => determineIf2Or3Or5(s, decodedByLength(1), sixAndNineAndZero(9)) -> s.toSet)
        .toMap

    (decodedByLength ++ sixAndNineAndZero ++ twoAndThreeAndFive).toMap.map(_.swap)

  def part2: Int =
    input.map { entry =>
      val codec = buildCodec(entry.signalPattern)
      val decoded = entry.outputValue.values.map(s => codec(s.toSet))
      decoded.mkString.toInt
    }.sum
