package com.rtjfarrimond.aoc2021.day4

import scala.io.Source


@main
def runDay4(): Unit =
  println(s"Part one: ${Day4.part1}")
  println(s"Part two: ${Day4.part2}")


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

  def part1: Int =
    val winState = drawUntilWin(BingoGame(cards), draw)
    if (winState._2.length > 1) throw new RuntimeException("More than one winner")
    else winState._2.head.score * winState._1

  @scala.annotation.tailrec
  def drawUntilWin(bingoGame: BingoGame, draw: List[Int]): (Int, List[Card]) =
    val thisDraw = draw.head
    val drawResult = bingoGame.draw(thisDraw)
    val newBingoGame = drawResult._2
    val winningCards = newBingoGame.winningCards
    if (winningCards.length > 0) (thisDraw, winningCards)
    else drawUntilWin(newBingoGame, draw.tail)

  def part2: Int =
    val last = drawUntilLast(BingoGame(cards), draw)
    val bingoGameWithLastCard = BingoGame(List(last._2))
    val winState = drawUntilWin(bingoGameWithLastCard, last._1)
    winState._2.head.score * winState._1

  def drawUntilLast(bingoGame: BingoGame, draw: List[Int]): (List[Int], Card) =
    val thisDraw = draw.head
    val drawResult = bingoGame.draw(thisDraw)
    val newBingoGame = drawResult._2
    val remainingCards = newBingoGame.remainingCards
    if (remainingCards.length == 1) (draw.tail, remainingCards.head)
    else drawUntilLast(newBingoGame, draw.tail)

}
