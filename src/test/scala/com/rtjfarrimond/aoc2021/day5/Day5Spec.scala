package com.rtjfarrimond.aoc2021.day5

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class Day5Spec extends AnyFlatSpec with Matchers {

  "part 1" must "work" in {
    val expected = 5
    val actual = Day5.part1

    actual mustBe expected
  }

  "part 2" must "work" ignore {
    val expected = 42
    val actual = Day5.part2

    actual mustBe expected
  }

}
