package net.halenka.hannes.aoc19scala

import scala.io.Source
import scala.util.{Failure, Success, Using}

object Day01 {
  def calcFuelRequirementModule(mass: Int): Int = (mass / 3) - 2

  def moduleMasses: Seq[Int] = {
    Using(Source.fromResource("day01/input.txt")) {
      resource => resource.getLines().toSeq
    } match {
      case Success(lines) => lines.map(_.toInt)
      case Failure(exception) => throw exception
    }
  }

  def answer: Day01Answer = {
    val fuelRequirement = moduleMasses.fold(0)(_ + calcFuelRequirementModule(_))

    Day01Answer(fuelRequirement)
  }
}

case class Day01Answer(fuelRequirements: Int)