package net.halenka.hannes.aoc19scala.intcode

import net.halenka.hannes.aoc19scala.intcode.ParameterMode.Position

case class Parameter(value: Int, mode: ParameterMode)

object Parameter {
  /** Returns a new `Parameter` instance in position mode. */
  def apply(value: Int): Parameter = Parameter(value, Position)
}

sealed trait ParameterMode

object ParameterMode {

  case object Position extends ParameterMode

}