package com.rtjfarrimond.aoc2021.day6

case class School(fish: List[LanternFish])

object School:
  
  extension(school: School)
    def ageOneDay: School =
      val newFish = school.fish.flatMap(_.ageOneDay)
      School(newFish)
