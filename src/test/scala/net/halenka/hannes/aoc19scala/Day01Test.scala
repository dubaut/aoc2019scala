import org.scalatest.flatspec.AnyFlatSpec

package net.halenka.hannes.aoc19scala {

  import net.halenka.hannes.aoc19scala.Day01._

  class Day01Test extends AnyFlatSpec {
    "A module of mass 12" should "require 2 fuel." in {
      assert(calcFuelRequirements(12) == 2)
    }

    "A module of mass 14" should "require 2 fuel." in {
      assert(calcFuelRequirements(14) == 2)
    }

    "A module of mass 1969" should "require 966 fuel." in {
      assert(calcFuelRequirements(1969) == 966)
    }

    "A module of mass 100756" should "require 50346 fuel." in {
      assert(calcFuelRequirements(100756) == 50346)
    }

    "All modules" should "require 4907345 fuel." in {
      assert(calcFuelRequirementsForModules == 4907345)
    }

    "The answer for Day 1 " should "be 4907345." in {
      assert(answer == Day01Answer(4907345))
    }
  }
}