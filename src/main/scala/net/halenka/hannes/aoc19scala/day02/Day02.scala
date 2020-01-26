package net.halenka.hannes.aoc19scala.day02

import net.halenka.hannes.aoc19scala.intcode.{Intcode, Program}
import net.halenka.hannes.aoc19scala._

import scala.util.{Failure, Success}

object Day02 {
  def calculateAnswer(program: Program): Either[Any, Int] = {
    val intcode = Intcode(program)

    (0 to 99)
      .foreach(noun =>
        (0 to 99)
          .withFilter(verb => intcode.run(noun, verb).head == 19690720)
          .foreach(_ => {})
      )

    for (noun <- 0 to 99) {
      for (verb <- 0 to 99) {
        if (intcode.run(noun, verb).head == 19690720)
          return Right(100 * noun + verb)
      }
    }

    Left("Could not determine pair of inputs.")
  }

  def answer: Either[Any, Day02Result] = {
    loadResourceAsIntSeq("day02/input.txt") match {
      case Success(programSteps) =>
        val program = Program(programSteps)

        val part1 = Intcode(program).run(12, 2).head
        val result = Day02Result(part1, _)

        calculateAnswer(program) match {
          case Right(part2) => Right(result(part2))
          case Left(left) => Left(left)
        }

      case Failure(ex) => Left(ex)
    }
  }
}

case class Day02Result(part1: Int, part2: Int)