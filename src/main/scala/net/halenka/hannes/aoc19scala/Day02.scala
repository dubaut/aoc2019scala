package net.halenka.hannes.aoc19scala

import net.halenka.hannes.aoc19scala.validation.NonEmptySeq._
import net.halenka.hannes.aoc19scala.validation._

import scala.util.{Failure, Success, Try}

object Day02 {
  val instructionLength = 4

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

  def answer: Day02Answer = {
    val sequence = loadResourceAsIntSeq("day02/input.txt")

    val answer = sequence match {
      case Success(seq) =>
        val adjustedProgram = seq.updated(1, 12).updated(2, 2)
        runIntcode(adjustedProgram)
      case Failure(ex) => throw ex
    }

    Day02Answer(answer.head)
  }

  @scala.annotation.tailrec
  private def processInstruction(program: NonEmptySeq[Int], step: Int): Seq[Int] = {
    assert(program != null)
    assert(step >= 0)

    val instruction = program.slice(instructionLength * step, instructionLength * step + instructionLength)
    val opcode = instruction.head
    opcode match {
      case 1 =>
        val modifiedProgram = processOpcode(Positions(instruction(1), instruction(2), instruction(3)), program, (a: Int, b: Int) => a + b)
        processInstruction(modifiedProgram.nes, step + 1)
      case 2 =>
        val modifiedProgram = processOpcode(Positions(instruction(1), instruction(2), instruction(3)), program, (a: Int, b: Int) => a * b)
        processInstruction(modifiedProgram.nes, step + 1)
      case 99 => program
      case _ => throw new RuntimeException(s"Illegal opcode: <{$opcode}>")
    }
  }

  private def processOpcode(positions: Positions, program: NonEmptySeq[Int], f: (Int, Int) => Int): Seq[Int] = {
    assert(positions != null)
    assert(program != null)
    assert(f != null)

    val value1 = program(positions.readPosition1)
    val value2 = program(positions.readPosition2)

    val result = f(value1, value2)

    program.take(positions.storePosition) :+ result :++ program.drop(positions.storePosition + 1)

  }

  def runIntcode(program: Seq[Int]): Seq[Int] = {
    val _program = program.requireNonEmpty("`program` must not be <null> or empty.")
    processInstruction(_program, 0)
  }
}

case class Day02Answer(value: Int)

case class Positions(readPosition1: Int, readPosition2: Int, storePosition: Int)