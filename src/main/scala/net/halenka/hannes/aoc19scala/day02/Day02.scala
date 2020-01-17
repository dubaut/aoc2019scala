package net.halenka.hannes.aoc19scala.day02

import net.halenka.hannes.aoc19scala.intcode.Intcode
import net.halenka.hannes.aoc19scala.loadTextFileResource
import net.halenka.hannes.aoc19scala.validation._

import scala.util.{Failure, Success, Try}

object Day02 {
  /**
   * @throws IllegalArgumentException if `resource` is blank.
   */
  def loadResourceAsIntSeq(resource: String): Try[IndexedSeq[Int]] = {
    val _resource = resource.requireNonBlank("`resource` must not be blank.")

    loadTextFileResource(_resource) match {
      case Success(lines) => Try(
        if (lines.nonEmpty) {
          lines.head.split(',').map(_.toInt).toIndexedSeq
        } else {
          IndexedSeq()
        }
      )
      case Failure(ex) => Failure(ex)
    }
  }

  def calculateAnswer(program: IndexedSeq[Int]): Either[Any, Int] = {
    val intcode = Intcode(program)
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
      case Success(program) =>
        val part1 = Intcode(program).run(12, 2).head
        val result = Day02Result(part1, _)

        calculateAnswer(program) match {
          case Right(part2) => Right(result(part2))
          case Left(left) =>Left(left)
        }

      case Failure(ex) => Left(ex)
    }
  }
}

case class Day02Result(part1: Int, part2: Int)