package com.rtjfarrimond.aoc2021.day6

case class School(fish: Map[LanternFish, Long])

object School:

  def apply(fish: List[LanternFish]): School =
    val initialFishMap = fish.groupMapReduce(identity)(_ => 1L)(_ + _)
    School(initialFishMap)

  extension(school: School)
    def ageOneDay: School =
      val fishToUpdate = school.fish.filter((_, v) => v > 0L)
      val fishUpdates =
        fishToUpdate
          .toList
          .flatMap { case (fish, count) =>
            fish.ageOneDay.map(_ -> count)
          }
      val reducedFishUpdates =
        fishUpdates
          .groupMapReduce(
            (lanternFish, _) => lanternFish
          )(
            (_, count) => count
          )(
            _ + _
          )
      School(reducedFishUpdates)
