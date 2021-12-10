package com.rtjfarrimond.aoc2021

import scala.io.Source


@main
def runDay3(): Unit =
  println(s"Part one: ${Day3.part1}")
  // println(s"Part two: ${Day3.part2}")

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
      case false => "0"
      case true => "1"
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

  // def part2: Int =
  //   42

}
