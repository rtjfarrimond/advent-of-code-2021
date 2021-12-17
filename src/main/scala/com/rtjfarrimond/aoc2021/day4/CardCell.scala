package com.rtjfarrimond.aoc2021.day4


enum CellStatus:
  case Marked, Unmarked

case class CardCell(value: Int, status: CellStatus)

object CardCell {
  def unmarked(value: Int): CardCell = CardCell(value, CellStatus.Unmarked)
}

