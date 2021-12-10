package com.rtjfarrimond.aoc2021

import scala.io.Source


@main
def runDay3(): Unit =
  println(s"Part one: ${Day3.part1}")
  println(s"Part two: ${Day3.part2}")

object Day3 {

  val input: List[List[Boolean]] =
    Source.fromResource("Day3.txt")
      .getLines
      .toList
      .map(_.toList)
      .map { innerList =>
        innerList.map {
          case '0' => false
          case '1' => true
          case _ => throw new RuntimeException("Input is not binary")
        }
      }

  def parseInt(bools: List[Boolean]): Int =
    val binaryString = bools.map {
      case false => '0'
      case true => '1'
    }.mkString
    Integer.parseInt(binaryString, 2)

  def computeGammaRate(partitioned: List[(List[Boolean], List[Boolean])]): List[Boolean] =
    partitioned.map {
      case (trueBools, falseBools) =>
        if (trueBools.length == falseBools.length) {
          throw new RuntimeException("Gamma and epsilon computation require all inputs contain an odd number of bits.")
        } else {
          trueBools.length > falseBools.length
        }
    }

  def computeEpsilonRate(partitioned: List[(List[Boolean], List[Boolean])]): List[Boolean] =
    computeGammaRate(partitioned).map(_ == false)

  def part1: Int =
    val transposed = input.transpose
    val partitioned = transposed.map(_.partition(b => b == true))
    val gamma = parseInt(computeGammaRate(partitioned))
    val epsilon = parseInt(computeEpsilonRate(partitioned))
    gamma * epsilon

  enum SelectionCriteria(val defaultValue: Boolean):
    case MostCommon   extends SelectionCriteria(true)
    case LeastCommon  extends SelectionCriteria(false)
  private def computeLifeSupportMetric(bools: List[List[Boolean]], selectionCriteria: SelectionCriteria): Int =

    def findMostCommon(input: List[Boolean]): Option[Boolean] =
      val partitioned: (List[Boolean], List[Boolean]) = input.partition(_ == true)
      val trueBools = partitioned._1
      val falseBools = partitioned._2
      if (trueBools.length == falseBools.length) None
      else Some(trueBools.length > falseBools.length)

    def loop(idx: Int, bools: List[List[Boolean]]): Int =
      if (bools.length == 1) parseInt(bools.head)
      else {
        val transposed = bools.transpose
        val mostCommon = findMostCommon(transposed(idx))
        val selectedBit = mostCommon match {
          case None =>
            selectionCriteria.defaultValue
          case Some(bit) =>
            selectionCriteria match {
              case SelectionCriteria.MostCommon =>
                bit
              case SelectionCriteria.LeastCommon =>
                !bit
            }
        }
        val filtered = bools.filter(l => l(idx) == selectedBit)
        loop(idx + 1, filtered)
      }

    loop(0, bools)

  def part2: Int =
    val oxygenGeneratorRating = computeLifeSupportMetric(input, SelectionCriteria.MostCommon)
    val c02SrubberRating = computeLifeSupportMetric(input, SelectionCriteria.LeastCommon)
    oxygenGeneratorRating * c02SrubberRating

}
