package com.rtjfarrimond.aoc2021

import scala.io.Source

enum Direction:
  case Forward, Down, Up

case class Instruction(direction: Direction, distance: Int)

case class Position(horizontal: Int, depth: Int) {
  val product = horizontal * depth
}
object Position {
  import Direction._
  val zero: Position = Position(0, 0)
  def +(p1: Position, p2: Position): Position =
    val newHorizontal = p1.horizontal + p2. horizontal
    val newDepth = p1.depth + p2.depth
    Position(newHorizontal, newDepth)

  def delta(instruction: Instruction): Position =
    instruction.direction match {
      case Forward =>
        Position(instruction.distance, 0)
      case Down =>
        Position(0, instruction.distance)
      case Up =>
        Position(0, -instruction.distance)
    }
}

@main
def runDay2(): Unit =
  println(s"Part one: ${Day2.part1()}")
  // println(s"Part two: ${Day2.part2()}")

object Day2 {

  val instructions: List[Instruction] =
    Source.fromResource("Day2.txt")
      .getLines
      .toList
      .map(parseInstruction)

  def parseInstruction(line: String): Instruction =
    val split = line.split(' ')
    val direction = Direction.valueOf(split(0).capitalize)
    val distance = split(1).toInt
    Instruction(direction, distance)

  def part1(): Int =
    val finalPosition =
      instructions
        .map(Position.delta)
        .fold(Position.zero)(Position.+)
    finalPosition.product

  def part2(): Int = ???

}
