package com.rtjfarrimond.aoc2021.day9

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

import scala.io.Source

class Day9Spec extends AnyFlatSpec with Matchers {

  "part 1" must "work" in {
    val expected = 15
    val actual = Day9.part1

    actual mustBe expected
  }

  "part 2" must "work" ignore {
    val expected = -42
    val actual = Day9.part2

    actual mustBe expected
  }

  "HeightMap.heightAt" must "lookup the correct index" in {
    val index = 21
    val expectedHeight = 42
    val input = List.fill(index)(0) ++ List(expectedHeight) ++ List.fill(3)(0)

    val heightMap = HeightMap(input, 5)
    heightMap.heightAt(Coordinate(1, 4)) mustBe Some(expectedHeight)
  }

  it must "return None if index is out of bounds" in {
    val input = List.fill(25)(0)
    val heightMap = HeightMap(input, 5)
    heightMap.heightAt(Coordinate(42, 1)) mustBe None
  }

  it must "compute left, right, above, and below" in {
    val maxX = 3
    val input = (1 to 9).toList
    val heightMap = HeightMap(input, maxX)

    heightMap.heightAt(Coordinate(1, 1)) mustBe Some(5)

    heightMap.heightAbove(Coordinate(1, 1)) mustBe Some(2)
    heightMap.heightBelow(Coordinate(1, 1)) mustBe Some(8)
    heightMap.heightRightOf(Coordinate(1, 1)) mustBe Some(6)
    heightMap.heightLeftOf(Coordinate(1, 1)) mustBe Some(4)

    heightMap.heightAbove(Coordinate(1, 0)) mustBe None
    heightMap.heightBelow(Coordinate(1, 2)) mustBe None
    heightMap.heightLeftOf(Coordinate(0, 1)) mustBe None
    heightMap.heightRightOf(Coordinate(2, 1)) mustBe None
  }

  "HeightMap.isLowPoint" must "be true if coordinate is a low point" in {
    val maxY = 3
    val input = (1 to 9).toList
    val heightMap = HeightMap(input, maxY)

    val range = (0 to 2).toList
    val coordinates =
      for {
        i <- range
        j <- range
      } yield (Coordinate(i, j))

    val expected = List(true) ++ List.fill(8)(false)
    val actual = coordinates.map(heightMap.isLowPoint)

    expected mustBe actual
  }

  "HeightMap.lowPoints" must "work" in {
    val input: List[List[Int]] = Source.fromResource("Day9.txt")
      .getLines
      .toList
      .map { line =>
        line.map(_.toString.toInt).toList
      }

    val expected = List(1, 0, 5, 5)
    val maxX = input.head.length
    val heightMap = HeightMap(input.flatten, maxX)

    val coordinates =
      for {
        x <- (0 to 9)
        y <- (0 to 4)
      } yield (Coordinate(x, y))

    // coordinates.foreach(coord => println(s"$coord: ${heightMap.locations(coord)}"))

    println(heightMap.lowPoints)

    heightMap.lowPoints.values must contain theSameElementsAs expected
  }

}
