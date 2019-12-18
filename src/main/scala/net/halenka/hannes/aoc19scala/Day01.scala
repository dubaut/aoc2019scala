package net.halenka.hannes.aoc19scala

import scala.io.Source
import scala.util.Try

object Day01 {
  def calcFuelRequirementModule(mass: Int): Int = (mass / 3) - 2

  def moduleMasses: Seq[Int] = {
    val source = Source.fromResource("day01/input.txt")

    try {
      source.getLines().map(_.toInt).toSeq
    } finally {
      Try(source.close())
    }
  }

  def answer: Day01Answer = {
    val fuelRequirement = moduleMasses.fold(0)(_ + calcFuelRequirementModule(_))

    Day01Answer(fuelRequirement)
  }
}

case class Day01Answer(fuelRequirements: Int)