package com.rtjfarrimond.aoc2021.day4


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

