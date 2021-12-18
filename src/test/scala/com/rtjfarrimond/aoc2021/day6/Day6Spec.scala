package com.rtjfarrimond.aoc2021.day6

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class Day6Spec extends AnyFlatSpec with Matchers {

  "part 1" must "work for 18 days" in {
    val expected = 26
    val actual = Day6.part1(18)

    actual mustBe expected
  }

  it must "work for 80 days" in {
    val expected = 5934
    val actual = Day6.part1(80)

    actual mustBe expected
  }

  "part 2" must "work" in {
    val expected = 26984457539L
    val actual = Day6.part2

    actual mustBe expected
  }

}

