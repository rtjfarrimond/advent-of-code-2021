package com.rtjfarrimond.aoc2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class Day1Spec extends AnyFlatSpec with Matchers {

  "part 1" must "work" in {
    val expected = 7
    val actual = Day1.part1()

    actual mustBe expected
  }

  "part 2" must "work" in {
    val expected = 5
    val actual = Day1.part2()

    actual mustBe expected
  }

}
