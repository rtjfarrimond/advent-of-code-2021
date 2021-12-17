package com.rtjfarrimond.aoc2021.day4


case class Card(cells: List[CardCell]) {

  lazy val score =
    val unmarked = cells.filter(_.status == CellStatus.Unmarked)
    unmarked.map(_.value).sum

  val hasWon: Boolean =
    val rows = cells.sliding(5, 5).toList
    hasWinningList(rows) || hasWinningList(rows.transpose)

  private[this] def hasWinningList(lists: List[List[CardCell]]): Boolean =
    lists.map { cells =>
      cells.forall(cell => cell.status == CellStatus.Marked)
    }.contains(true)

}

object Card {

  def unmarked(ints: List[Int]): Card =
    val cells = ints.map(CardCell.unmarked)
    Card(cells)

  extension(card: Card)

    def mark(value: Int): Card =
      val newCells = card.cells.map { cell =>
        if (cell.value == value) CardCell(value, CellStatus.Marked)
        else cell
      }
      Card(newCells)

}
