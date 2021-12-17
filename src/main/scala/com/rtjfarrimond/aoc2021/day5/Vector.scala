package com.rtjfarrimond.aoc2021.day5

case class Vector(from: Coordinate, to: Coordinate) {

  val isDiagonal: Boolean = from.x != to.x && from.y != to.y

  override def toString: String = s"${from.x},${from.y} -> ${to.x},${to.y}"

}

