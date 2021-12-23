package com.rtjfarrimond.aoc2021.day7

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class Day7Spec extends AnyFlatSpec with Matchers {

  "part 1" must "work" in {
    val expected = 37
    val actual = Day7.part1

    actual mustBe expected
  }

  "getFuelCost" must "work when from > to" in {
    val from = 10
    val to = 2

    Day7.getFuelCost(from, to) mustBe 8
  }

  it must "work when from < to" in {
    val from = 2
    val to = 10

    Day7.getFuelCost(from, to) mustBe 8
  }

  "part 2" must "work" in {
    val expected = 168
    val actual = Day7.part2

    actual mustBe expected
  }

}

