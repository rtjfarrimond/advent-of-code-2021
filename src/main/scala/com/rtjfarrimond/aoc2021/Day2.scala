package com.rtjfarrimond.aoc2021

import scala.io.Source

enum Direction:
  case Forward, Down, Up

case class Instruction(direction: Direction, distance: Int)

case class Position(horizontal: Int, depth: Int) {
  val product = horizontal * depth
}

case class Submarine(position: Position) {

  import Direction._

  def move(instruction: Instruction): Submarine =
    val newPos = instruction.direction match {
      case Forward =>
        val horizontal = this.position.horizontal + instruction.distance
        Position(horizontal, this.position.depth)
      case Down =>
        val depth = this.position.depth + instruction.distance
        Position(this.position.horizontal, depth)
      case Up =>
        val depth = this.position.depth - instruction.distance
        Position(this.position.horizontal, depth)
    }
    Submarine(newPos)

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
    val initialSubmarine = Submarine(Position(0, 0))
    val moved = instructions
      .foldLeft[Submarine](initialSubmarine) {
        case (submarine, instruction) =>
          submarine.move(instruction)
      }
    moved.position.product

  def part2(): Int = ???

}
