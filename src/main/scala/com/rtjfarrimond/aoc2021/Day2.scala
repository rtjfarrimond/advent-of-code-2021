package com.rtjfarrimond.aoc2021

import scala.io.Source

enum Direction:
  case Forward, Down, Up

case class Instruction(direction: Direction, magnitude: Int)

trait Positional {
  def horizontal: Int
  def depth: Int
  def product: Int = horizontal * depth
}

trait Navigator[P <: Positional] {
  def zero: Submarine[P]
  def move(submarine: Submarine[P], instruction: Instruction): Submarine[P]
}

case class Position(horizontal: Int, depth: Int) extends Positional
case class AimedPosition(horizontal: Int, depth: Int, aim: Int) extends Positional

case class Submarine[P <: Positional](position: P) {

  import Direction._

  def move(instruction: Instruction)(using navigator: Navigator[P]): Submarine[P] =
    navigator.move(this, instruction)

}

object Position {

  import Direction._

  given Navigator[Position] with

    override val zero: Submarine[Position] = Submarine(Position(0, 0))

    override def move(submarine: Submarine[Position], instruction: Instruction): Submarine[Position] =
      val newPos = instruction.direction match {
        case Forward =>
          val horizontal = submarine.position.horizontal + instruction.magnitude
          Position(horizontal, submarine.position.depth)
        case Down =>
          val depth = submarine.position.depth + instruction.magnitude
          Position(submarine.position.horizontal, depth)
        case Up =>
          val depth = submarine.position.depth - instruction.magnitude
          Position(submarine.position.horizontal, depth)
      }
      Submarine(newPos)

}

object AimedPosition {

  import Direction._

  given Navigator[AimedPosition] with

    override val zero: Submarine[AimedPosition] = Submarine(AimedPosition(0, 0, 0))

    override def move(submarine: Submarine[AimedPosition], instruction: Instruction): Submarine[AimedPosition] =
      val newPos = instruction.direction match {
        case Down =>
          val newAim = submarine.position.aim + instruction.magnitude
          AimedPosition(submarine.position.horizontal, submarine.position.depth, newAim)
        case Up =>
          val newAim = submarine.position.aim - instruction.magnitude
          AimedPosition(submarine.position.horizontal, submarine.position.depth, newAim)
        case Forward =>
          val newHorizontal = submarine.position.horizontal + instruction.magnitude
          val newDepth = submarine.position.depth + (submarine.position.aim * instruction.magnitude)
          AimedPosition(newHorizontal, newDepth, submarine.position.aim)
      }
      Submarine(newPos)

}

@main
def runDay2(): Unit =
  println(s"Part one: ${Day2.part1}")
  println(s"Part two: ${Day2.part2}")

object Day2 {

  val instructions: List[Instruction] =
    Source.fromResource("Day2.txt")
      .getLines
      .toList
      .map(parseInstruction)

  def parseInstruction(line: String): Instruction =
    val split = line.split(' ')
    val direction = Direction.valueOf(split(0).capitalize)
    val magnitude = split(1).toInt
    Instruction(direction, magnitude)

  def pilot[P <: Positional](using nav: Navigator[P]): Int =
    val initialSubmarine = nav.zero
    val moved = instructions
      .foldLeft[Submarine[P]](initialSubmarine) {
        case (submarine, instruction) =>
          submarine.move(instruction)
      }
    moved.position.product

  def part1(using nav: Navigator[Position]): Int =
    pilot

  def part2(using nav: Navigator[AimedPosition]): Int =
    pilot

}
