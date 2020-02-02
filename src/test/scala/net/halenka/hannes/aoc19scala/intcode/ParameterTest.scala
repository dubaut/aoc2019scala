package net.halenka.hannes.aoc19scala.intcode

import net.halenka.hannes.aoc19scala.intcode.Parameter.ParameterMode.Position
import org.scalatest.flatspec.AnyFlatSpec

class ParameterTest extends AnyFlatSpec {
  "`Parameter(Int)`" must "return a new `Parameter` instance in position mode." in {
    val value = 1

    assertResult(Parameter(value, Position))(Parameter(value))
  }
}