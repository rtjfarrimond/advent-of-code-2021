package com.rtjfarrimond.aoc2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class Day2Spec extends AnyFlatSpec with Matchers {

  "part 1" must "work" in {
    val expected = 150
    val actual = Day2.part1

    actual mustBe expected
  }

  "part 2" must "work" in {
    val expected = 900
    val actual = Day2.part2

    actual mustBe expected
  }

}
