package net.halenka.hannes.aoc19scala.day03

import net.halenka.hannes.aoc19scala.validation.StringValidator

case class WiringInstruction(direction: Direction, distance: Int)

object WiringInstruction {
  private val instructionPattern = "^([LRUD])([1-9][0-9]*)$".r

  def apply(instruction: String): WiringInstruction = {
    val _instruction = instruction.requireNonBlank("The mandatory parameter <instruction> must not be blank.")

    if (instructionPattern.matches(_instruction) == false) {
      throw new RuntimeException(s"Cannot parse instruction: '${instruction}'.")
    }

    val instructionPattern(direction, distance) = instruction

    direction match {
      case "L" => new WiringInstruction(LEFT, distance.toInt)
      case "R" => new WiringInstruction(RIGHT, distance.toInt)
      case "U" => new WiringInstruction(UP, distance.toInt)
      case "D" => new WiringInstruction(DOWN, distance.toInt)
    }
  }
}

sealed trait Direction

case object LEFT extends Direction

case object RIGHT extends Direction

case object UP extends Direction

case object DOWN extends Direction