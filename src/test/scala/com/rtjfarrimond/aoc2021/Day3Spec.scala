package com.rtjfarrimond.aoc2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class Day3Spec extends AnyFlatSpec with Matchers {

  "part 1" must "work" in {
    val expected = 198
    val actual = Day3.part1

    actual mustBe expected
  }

  "parseInt" must "convert a list of booleans to a base 10 decimal" in {
    val bools = List(false, false, true, true)
    val expected = 3
    val actual = Day3.parseInt(bools)
    actual mustBe expected
  }

  "part 2" must "work" in {
    val expected = 230
    val actual = Day3.part2

    actual mustBe expected
  }

}
