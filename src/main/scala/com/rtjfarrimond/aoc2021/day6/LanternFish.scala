package com.rtjfarrimond.aoc2021.day6

sealed trait LanternFish {
  def daysUntilBirth: Int
}

object LanternFish:

  def apply(daysUntilBirth: Int): LanternFish = AdultLanternFish(daysUntilBirth)

  private case class AdultLanternFish(daysUntilBirth: Int) extends LanternFish
  private case class YoungLanternFish(daysUntilBirth: Int) extends LanternFish

  private val gestationPeriod = 6
  def mommy: LanternFish = AdultLanternFish(gestationPeriod)

  private val initialGestationPeriod = 8
  def baby: LanternFish = YoungLanternFish(initialGestationPeriod)

  extension(lanternFish: LanternFish)
    def ageOneDay: List[LanternFish] = lanternFish match {
      case YoungLanternFish(age) if age == 0 =>
        List(AdultLanternFish(gestationPeriod), LanternFish.baby)
      case YoungLanternFish(age) =>
        List(YoungLanternFish(age - 1))
      case AdultLanternFish(age) if age > 0 =>
        List(AdultLanternFish(age - 1))
      case AdultLanternFish(age) =>
        List(LanternFish.mommy, LanternFish.baby)
    }
