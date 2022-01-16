package com.rtjfarrimond.aoc2021.day10

import scala.collection.mutable.Stack
import scala.io.Source


@main
def runDay10(): Unit =
  println(s"Part one: ${Day10.part1}")
  println(s"Part two: ${Day10.part2}")


object Day10:

  val closeCharLookup = Map[Char, Char](
    '(' -> ')',
    '[' -> ']',
    '<' -> '>',
    '{' -> '}'
  )

  case class Corruption(expected: Char, actual: Char)
  type IncompleteLine = Stack[Char]
  type AnalysisResult = Either[IncompleteLine, Corruption]

  val analysed: List[AnalysisResult] = Source.fromResource("Day10.txt")
    .getLines
    .toList
    .map(analyseLine)

  def analyseLine(line: String): AnalysisResult =

    def opensChunk(char: Char): Boolean =
      char match {
        case '{' => true
        case '(' => true
        case '[' => true
        case '<' => true
        case _ => false
      }

    def closesChunk(previous: Char, current: Char): Boolean =
      previous match {
        case '(' if current == ')' => true
        case '{' if current == '}' => true
        case '[' if current == ']' => true
        case '<' if current == '>' => true
        case _ => false
      }

    val stack = Stack.empty[Char]

    @scala.annotation.tailrec
    def loop(line: String): AnalysisResult =
      if (line == "")
        Left(stack)
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
          Right(Corruption(expected, top))

    loop(line)


  def part1: Int =

    val scoreLookup = Map[Char, Int](
      ')' -> 3,
      ']' -> 57,
      '}' -> 1197,
      '>' -> 25137
    )

    analysed
      .collect {
        case Right(corruption) =>
          scoreLookup(corruption.actual)
      }
      .sum


  def part2: Long =

    val pointLookup = Map[Char, Long](
      ')' -> 1,
      ']' -> 2,
      '}' -> 3,
      '>' -> 4
    )

    val scores =
      analysed
        .collect {
          case Left(incompleteLine) =>
            incompleteLine
              .map(closeCharLookup)
              .map(pointLookup)
        }
        .map { points =>
          points.foldLeft(0L) { (acc, next) =>
            acc * 5L + next
          }
        }
        .sorted

    scores(scores.length / 2)
