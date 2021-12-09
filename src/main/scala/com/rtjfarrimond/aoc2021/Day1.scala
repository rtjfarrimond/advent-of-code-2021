package com.rtjfarrimond.aoc2021

import scala.io.Source

@main
def runDay1(): Unit =
  println(s"Part one: ${Day1.part1()}")
  println(s"Part two: ${Day1.part2()}")

object Day1 {

  val input = Source.fromResource("Day1.txt")
    .getLines
    .toList
    .map(_.toInt)

  def part1(): Int =
    loop(0, input)

  def part2(): Int =
    val sliding = input.sliding(3)
    loop(0, sliding.toList.map(_.sum))

  @scala.annotation.tailrec
  def loop(acc: Int, input: List[Int]): Int =
    if (input.length >= 2) {
      val first = input(0)
      val second = input(1)
      if (second > first)
        loop(acc + 1, input.tail)
      else
        loop(acc, input.tail)
    } else {
      acc
    }

}
