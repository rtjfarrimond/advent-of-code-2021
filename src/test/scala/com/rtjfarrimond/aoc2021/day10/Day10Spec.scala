package com.rtjfarrimond.aoc2021.day10

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

import scala.io.Source

class Day10Spec extends AnyFlatSpec with Matchers {

  "part 1" must "work" in {
    val expected = 26397
    val actual = Day10.part1

    actual mustBe expected
  }

  "part 2" must "work" in {
    val expected = 288957
    val actual = Day10.part2

    actual mustBe expected
  }

}
