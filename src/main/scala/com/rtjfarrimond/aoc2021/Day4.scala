package com.rtjfarrimond.aoc2021

import scala.io.Source


@main
def runDay4(): Unit =

  // TODO: Should we be using sets rather than lists throughout?

  println(s"Part one: ${Day4.part1}")
  println(s"Part two: ${Day4.part2}")


enum CellStatus:
  case Marked, Unmarked

case class CardCell(value: Int, status: CellStatus)
object CardCell {
  def unmarked(value: Int): CardCell = CardCell(value, CellStatus.Unmarked)
}

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

case class BingoGame(cards: List[Card]) {
  val winningCards: List[Card] = cards.filter(_.hasWon == true)
  val remainingCards: List[Card] = cards.filter(_.hasWon == false)
}

object BingoGame {

  extension(bingoGame: BingoGame)
    def draw(int: Int): (Int, BingoGame) =
      val markedCards = bingoGame.cards.map(_.mark(int))
      val nextGame = BingoGame(markedCards)
      (int, nextGame)

}

object Day4 {

  val input: List[String] = Source.fromResource("Day4.txt")
    .getLines
    .toList

  val draw: List[Int] = input.head.split(',').map(_.toInt).toList
  val cards: List[Card] = {
    val noBlankLines = input.tail.filterNot(_ == "").map(_.trim)
    val windowed: List[List[String]] = noBlankLines.sliding(5, 5).toList

    windowed.map { innerList =>
      val intLists = innerList.map(line => line.split("\\s+").toList.map(_.toInt))
      Card.unmarked(intLists.flatten)
    }
  }

  @scala.annotation.tailrec
  def drawUntilWin(bingoGame: BingoGame, draw: List[Int]): (Int, List[Card]) =
    val thisDraw = draw.head
    val drawResult = bingoGame.draw(thisDraw)
    val newBingoGame = drawResult._2
    val winningCards = newBingoGame.winningCards
    if (winningCards.length > 0) (thisDraw, winningCards)
    else drawUntilWin(newBingoGame, draw.tail)

  def part1: Int =
    val winState = drawUntilWin(BingoGame(cards), draw)
    if (winState._2.length > 1) throw new RuntimeException("More than one winner")
    else winState._2.head.score * winState._1

  def drawUntilLast(bingoGame: BingoGame, draw: List[Int]): (List[Int], Card) =
    val thisDraw = draw.head
    val drawResult = bingoGame.draw(thisDraw)
    val newBingoGame = drawResult._2
    val remainingCards = newBingoGame.remainingCards
    if (remainingCards.length == 1) (draw.tail, remainingCards.head)
    else drawUntilLast(newBingoGame, draw.tail)


  def part2: Int =
    val last = drawUntilLast(BingoGame(cards), draw)
    val bingoGameWithLastCard = BingoGame(List(last._2))
    val winState = drawUntilWin(bingoGameWithLastCard, last._1)
    winState._2.head.score * winState._1

}
