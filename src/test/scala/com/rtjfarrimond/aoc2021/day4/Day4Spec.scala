package com.rtjfarrimond.aoc2021.day4

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class Day4Spec extends AnyFlatSpec with Matchers {

  "part 1" must "work" in {
    val expected = 4512
    val actual = Day4.part1

    actual mustBe expected
  }

  "Card.hasWon" must "be false when all cells are unmarked" in {
    val card = Card.unmarked((1 to 25).toList)
    card.hasWon mustBe false
  }

  it must "be false when there is no complete row or column but some cells are marked" in {
    val card = Card.unmarked((1 to 25).toList).mark(1)
    card.hasWon mustBe false
  }

  it must "be true when a whole row is marked" in {
    val card = Card.unmarked((1 to 25).toList)
      .mark(1)
      .mark(2)
      .mark(3)
      .mark(4)
      .mark(5)
    card.hasWon mustBe true
  }

  it must "be true when a whole column is marked" in {
    val card = Card.unmarked((1 to 25).toList)
      .mark(5)
      .mark(10)
      .mark(15)
      .mark(20)
      .mark(25)
    card.hasWon mustBe true
  }

  "part 2" must "work" in {
    val expected = 1924
    val actual = Day4.part2

    actual mustBe expected
  }

}
