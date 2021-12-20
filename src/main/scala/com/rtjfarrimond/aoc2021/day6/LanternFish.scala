package com.rtjfarrimond.aoc2021.day6

sealed trait LanternFish {
  def daysUntilBirth: Int
}

object LanternFish:

  def adult(daysUntilBirth: Int): LanternFish =
    AdultLanternFish(daysUntilBirth)

  private case class AdultLanternFish(daysUntilBirth: Int) extends LanternFish
  private case class YoungLanternFish(daysUntilBirth: Int) extends LanternFish

  private val gestationPeriod    = 6
  private val mommy: LanternFish = AdultLanternFish(gestationPeriod)

  private val initialGestationPeriod = 8
  private val baby: LanternFish      = YoungLanternFish(initialGestationPeriod)

  extension(lanternFish: LanternFish)
    def ageOneDay: List[LanternFish] = lanternFish match {
      case lanternFish if lanternFish.daysUntilBirth == 0 =>
        List(LanternFish.mommy, LanternFish.baby)
      case YoungLanternFish(daysUntilBirth) =>
        List(YoungLanternFish(daysUntilBirth - 1))
      case AdultLanternFish(daysUntilBirth) =>
        List(AdultLanternFish(daysUntilBirth - 1))
    }
