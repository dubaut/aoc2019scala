package net.halenka.hannes.aoc19scala.day03

import org.apache.commons.lang3.StringUtils
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class WiringInstructionTest extends AnyFlatSpec with Matchers {
  private val instructionR8 = WiringInstruction("R8")
  private val instructionU5 = WiringInstruction("U5")
  private val instructionL5 = WiringInstruction("L5")
  private val instructionD3 = WiringInstruction("D3")

  "`WiringInstruction(String)`" should "return a new `WiringInstruction` instance." in {
    WiringInstruction("R8") shouldBe a[WiringInstruction]
  }

  it should "produce an `IllegalArgumentException` if `instruction` is either `null` or blank." in {
    assertThrows[IllegalArgumentException] {
      WiringInstruction(null)
    }

    assertThrows[IllegalArgumentException] {
      WiringInstruction(StringUtils.EMPTY)
    }

    assertThrows[IllegalArgumentException] {
      WiringInstruction(" \n")
    }
  }

  it should "produce a `RuntimeException` if the instruction could not be parsed." in {
    assertThrows[RuntimeException] {
      WiringInstruction("CAN_NOT_BE_PARSED")
    }
  }

  "A `WiringInstruction` created from 'R8'" should "have the direction `Right`" in {
    assertResult(RIGHT)(instructionR8.direction)
  }

  it should "have the distance `8`" in {
    assertResult(8)(instructionR8.distance)
  }

  "A `WiringInstruction` created from 'U5'" should "have the direction `Up`" in {
    assertResult(UP)(instructionU5.direction)
  }

  it should "have the distance `5`" in {
    assertResult(5)(instructionU5.distance)
  }

  "A `WiringInstruction` created from 'L5'" should "have the direction `Left`" in {
    assertResult(LEFT)(instructionL5.direction)
  }

  it should "have the distance `5`" in {
    assertResult(5)(instructionL5.distance)
  }

  "A `WiringInstruction` created from 'D3'" should "have the direction `Down`" in {
    assertResult(DOWN)(instructionD3.direction)
  }

  it should "have the distance `3`" in {
    assertResult(3)(instructionD3.distance)
  }
}
