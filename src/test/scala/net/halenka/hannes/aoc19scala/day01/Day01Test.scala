package net.halenka.hannes.aoc19scala.day01

import net.halenka.hannes.aoc19scala.day01.Day01._
import org.scalatest.flatspec.AnyFlatSpec

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
  }