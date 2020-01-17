package net.halenka.hannes.aoc19scala.day01

import scala.io.Source
import scala.util.{Failure, Success, Using}

object Day01 {
  def calcFuelRequirements(mass: Int): Int = {
    var fuelRequirement =  (mass / 3) - 2

    if (fuelRequirement > 0) {
      fuelRequirement = fuelRequirement + calcFuelRequirements(fuelRequirement)
    } else {
      fuelRequirement = 0
    }

    fuelRequirement
  }

  def moduleMasses: IndexedSeq[Int] = {
    Using(Source.fromResource("day01/input.txt")) {
      resource => resource.getLines().toSeq
    } match {
      case Success(lines) => lines.map(_.toInt).toIndexedSeq
      case Failure(exception) => throw exception
    }
  }

  def calcFuelRequirementsForModules: Int = {
    val fuelRequirement = moduleMasses.fold(0)(_ + calcFuelRequirements(_))
    fuelRequirement
  }

  def answer: Either[Any, Day01Result] = {
    val part1 = moduleMasses.fold(0)((a, b) => a + (b / 3) - 2)
    val part2 = calcFuelRequirementsForModules

    Right(Day01Result(part1, part2))
  }
}

case class Day01Result(part1: Int, part2: Int)