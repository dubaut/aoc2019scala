package net.halenka.hannes.aoc19scala.day05

import net.halenka.hannes.aoc19scala
import net.halenka.hannes.aoc19scala.intcode.{Intcode, Program}

import scala.util.{Failure, Success}

object Day05 {
  def answer: Either[Any, Day05Result] = {
    aoc19scala.loadResourceAsIntSeq("day05/input.txt") match {
      case Success(programSteps) =>
        val output = Intcode.run(Program(programSteps)) match {
          case (_, output) => output.foldLeft(new StringBuilder)((a, b) => a.append(b)).toString()
        }

        Right(Day05Result(output))
      case Failure(exception) => Left(exception)
    }
  }
}

case class Day05Result(part1: String)