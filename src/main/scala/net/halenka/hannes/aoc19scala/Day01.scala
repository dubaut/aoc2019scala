package net.halenka.hannes.aoc19scala

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

  def moduleMasses: Seq[Int] = {
    Using(Source.fromResource("day01/input.txt")) {
      resource => resource.getLines().toSeq
    } match {
      case Success(lines) => lines.map(_.toInt)
      case Failure(exception) => throw exception
    }
  }

  def calcFuelRequirementsForModules: Int = {
    val fuelRequirement = moduleMasses.fold(0)(_ + calcFuelRequirements(_))
    fuelRequirement
  }

  def answer: Day01Answer = Day01Answer(calcFuelRequirementsForModules)
}

case class Day01Answer(fuelRequirements: Int)