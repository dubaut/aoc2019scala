import org.scalatest.flatspec.AnyFlatSpec

package net.halenka.hannes.aoc19scala {

  import net.halenka.hannes.aoc19scala.Day01._

  class Day01Test extends AnyFlatSpec {
    "A module of mass 12" should "require 2 fuel." in {
      assert(calcFuelRequirementModule(12) == 2)
    }

    "A module of mass 14" should "require 2 fuel." in {
      assert(calcFuelRequirementModule(14) == 2)
    }

    "A module of mass 1969" should "require 654 fuel." in {
      assert(calcFuelRequirementModule(1969) == 654)
    }

    "A module of mass 100756" should "require 33583 fuel." in {
      assert(calcFuelRequirementModule(100756) == 33583)
    }

    "The answer for Day 1 " should "be 3273471." in {
      assert(answer == Day01Answer(3273471))
    }
  }
}