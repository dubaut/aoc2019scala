package net.halenka.hannes.aoc19scala

import net.halenka.hannes.aoc19scala.intcode.Intcode
import net.halenka.hannes.aoc19scala.validation._

import scala.util.{Failure, Success, Try}

object Day02 {
  /**
   * @throws IllegalArgumentException if `resource` is blank.
   */
  def loadResourceAsIntSeq(resource: String): Try[Seq[Int]] = {
    val _resource = resource.requireNonBlank("`resource` must not be blank.")

    loadTextFileResource(_resource) match {
      case Success(lines) => Try(
        if (lines.nonEmpty) {
          lines.head.split(',').map(_.toInt).toSeq
        } else {
          Nil
        }
      )
      case Failure(ex) => Failure(ex)
    }
  }

  def calculateAnswer(program: Seq[Int]): Either[Any, Int] = {
    val intcode = Intcode(program)
    for (noun <- 0 to 99) {
      for (verb <- 0 to 99) {
        if (intcode.run(noun, verb).head == 19690720)
          return Right(100 * noun + verb)
      }
    }

    Left("Could not determine pair of inputs.")
  }

  def answer: Either[Any, Int] = {
    loadResourceAsIntSeq("day02/input.txt") match {
      case Success(program) => calculateAnswer(program)
      case Failure(ex) => Left(ex)
    }
  }
}