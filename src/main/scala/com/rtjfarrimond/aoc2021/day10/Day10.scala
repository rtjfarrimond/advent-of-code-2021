package com.rtjfarrimond.aoc2021.day10

import scala.collection.mutable.Stack
import scala.io.Source


@main
def runDay10(): Unit =
  println(s"Part one: ${Day10.part1}")
  println(s"Part two: ${Day10.part2}")


object Day10:

  val input: List[String] = Source.fromResource("Day10.txt")
    .getLines
    .toList

  def closesChunk(previous: Char, current: Char): Boolean =
    previous match {
      case '(' if current == ')' => true
      case '{' if current == '}' => true
      case '[' if current == ']' => true
      case '<' if current == '>' => true
      case _ => false
    }

  val closeCharLookup = Map[Char, Char](
    '(' -> ')',
    '[' -> ']',
    '<' -> '>',
    '{' -> '}'
  )

  case class Corruption(expected: Char, actual: Char)

  def detectCorruption(line: String): Option[Corruption] =

    def opensChunk(char: Char): Boolean =
      char match {
        case '{' => true
        case '(' => true
        case '[' => true
        case '<' => true
        case _ => false
      }

    val stack = scala.collection.mutable.Stack.empty[Char]

    @scala.annotation.tailrec
    def loop(line: String): Option[Corruption] =
      if (line == "")
        None
      else if (stack.isEmpty)
        stack.push(line.head)
        loop(line.tail)
      else
        val top = stack.top
        val next = line.head
        if (closesChunk(top, next))
          stack.pop
          loop(line.tail)
        else if (opensChunk(top))
          stack.push(next)
          loop(line.tail)
        else
          val expected = closeCharLookup(stack(1))
          Some(Corruption(expected, top))

    loop(line)

  def part1: Int =

    val scoreLookup = Map[Char, Int](
      ')' -> 3,
      ']' -> 57,
      '}' -> 1197,
      '>' -> 25137
    )

    input
      .map(detectCorruption)
      .collect {
        case Some(corruption) =>
          scoreLookup(corruption.actual)
      }
      .sum


  def part2: Long =

    def isCorrupt(line: String): Boolean =
      detectCorruption(line).isDefined

    def getCompletion(line: String): String =
      val stack = Stack.empty[Char]

      @scala.annotation.tailrec
      def loop(line: String): String =
        if (line.isEmpty)
          stack.mkString.map(closeCharLookup)
        else if (stack.isEmpty)
          stack.push(line.head)
          loop(line.tail)
        else
          val top = stack.top
          val next = line.head
          if (closesChunk(top, next))
            stack.pop
            loop(line.tail)
          else // Opens a chunk
            stack.push(next)
            loop(line.tail)

      loop(line)

    val pointLookup = Map[Char, Long](
      ')' -> 1,
      ']' -> 2,
      '}' -> 3,
      '>' -> 4
    )

    val scores =
      input
        .filterNot(isCorrupt)
        .map(getCompletion)
        .map(completion => completion.map(pointLookup))
        .map { points =>
          points.foldLeft(0L) { (acc, next) =>
            acc * 5L + next
          }
        }
        .sorted

    scores(scores.length / 2)
