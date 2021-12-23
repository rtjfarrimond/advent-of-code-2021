package com.rtjfarrimond.aoc2021.day8

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class Day8Spec extends AnyFlatSpec with Matchers {

  "part 1" must "work" in {
    val expected = 26
    val actual = Day8.part1

    actual mustBe expected
  }

  "part 2" must "work" in {
    val expected = 61229
    val actual = Day8.part2

    actual mustBe expected
  }

}

