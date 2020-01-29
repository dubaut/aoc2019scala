package net.halenka.hannes.aoc19scala.intcode

import net.halenka.hannes.aoc19scala.intcode.Instruction.StoreInput
import net.halenka.hannes.aoc19scala.intcode.Intcode._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.{OptionValues, TryValues}

class IntcodeTest extends AnyFlatSpec with Matchers with TryValues with OptionValues {
  "`Intcode(Seq)`" must "return a new `Intcode` instance." in {
    Intcode(Program(99)) mustBe a[Intcode]
  }

  it should "produce an `IllegalArgumentException` if `program` is `null`." in {
    assertThrows[IllegalArgumentException](Intcode(null))
  }

  "`applyInstructionWithInput(..)`" must "return the effected program and its output." in {
    val instruction = StoreInput(Parameter(0))
    val program = Program(3, 0, 99)
    val input = 1

    val expected = (Program(1, 0, 99), None)
    val actual = applyInstructionWithInput(instruction, program, input)
    assertResult(expected)(actual)
  }

  it must "produce an `IllegalArgumentException` if `instruction` `null`" in {
    assertThrows[IllegalArgumentException] {
      applyInstructionWithInput(null, Program(99), 0)
    }
  }

  it must "produce an `IllegalArgumentException` if `program` is either `null`." in {
    assertThrows[IllegalArgumentException] {
      applyInstructionWithInput(StoreInput(Parameter(0)), null, 0)
    }
  }

  "`run(..)`" must "return the modified program and the output after processing all instructions." in {
    val emptyOutput = Nil.toIndexedSeq

    assertResult((IndexedSeq(2, 0, 0, 0, 99), emptyOutput))(Intcode.run(Program(1, 0, 0, 0, 99)))
    assertResult((IndexedSeq(2, 3, 0, 6, 99), emptyOutput))(Intcode.run(Program(2, 3, 0, 3, 99)))
    assertResult((IndexedSeq(2, 4, 4, 5, 99, 9801), emptyOutput))(Intcode.run(Program(2, 4, 4, 5, 99, 0)))
    assertResult((IndexedSeq(30, 1, 1, 4, 2, 5, 6, 0, 99), emptyOutput))(Intcode.run(Program(1, 1, 1, 4, 99, 5, 6, 0, 99)))
    assertResult((IndexedSeq(3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50), emptyOutput))(Intcode.run(Program(1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50)))

    assertResult((IndexedSeq(1, 1, 3, 1, 99), emptyOutput))(Intcode.run(Program(3, 0, 3, 1, 99)))
    //assertResult(IndexedSeq(1002, 4, 3, 4, 99), emptyOutput)(Intcode.run(IndexedSeq(1002, 4, 3, 4, 33)))
  }

  it must "produce an `IllegalArgumentException` if `program` is `null`." in {
    assertThrows[IllegalArgumentException] {
      Intcode.run(null)
    }
  }

  it must "produce an `IllegalArgumentException` if `address` is less than '0'." in {
    assertThrows[IllegalArgumentException] {
      Intcode.run(Program(99), -1)
    }
  }
}