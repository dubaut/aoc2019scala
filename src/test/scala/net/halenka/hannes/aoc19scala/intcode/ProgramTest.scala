package net.halenka.hannes.aoc19scala.intcode

import net.halenka.hannes.aoc19scala.intcode.Instruction.{Add, Multiply, StoreInput, Terminate}
import net.halenka.hannes.aoc19scala.intcode.ParameterMode.Position
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class ProgramTest extends AnyFlatSpec with Matchers {
  "`Program(IndexedSeq[Int)`" must "produce an `IllegalArgumentException` if `steps` is `null`." in {
    assertThrows[IllegalArgumentException] {
      Program(null)
    }
  }

  it must "produce an `IllegalArgumentException` if `steps` is empty." in {
    assertThrows[IllegalArgumentException] {
      Program(Nil.toIndexedSeq)
    }
  }

  "`Program(Int*)`" must "return a new `Program` instance with the specified steps." in {
    val expected = IndexedSeq(1, 0, 0, 0, 99)

    val program = Program(expected(0), expected(1), expected(2), expected(3), expected(4))
    val actual = program.steps

    assertResult(expected)(actual)
  }

  "`updated`" must "return a copy of the program with the value at the specified address replaced." in {
    val program = Program(1, 0, 0, 0, 99)

    val expected = Program(1, 0, 0, 1, 99)
    val actual = program.updated(3, 1)
    assertResult(expected)(actual)
  }

  it must "produce an `IllegalArgumentException` if `address` is less than `0`." in {
    assertThrows[IllegalArgumentException] {
      Program(99).updated(-1, 1)
    }
  }

  it must "produce an `IllegalArgumentException` if `address` is greater than the highest address in the program." in {
    assertThrows[IllegalArgumentException] {
      Program(99).updated(1, 1)
    }
  }

  "`size`" must "return the number of steps in a program." in {
    assertResult(1)(Program(99).size)
    assertResult(5)(Program(1, 0, 0, 0, 99).size)
  }

  "getInstruction(0) invoked on Program(1, 2, 5, 0)" must "return an Add instruction." in {
    val program = Program(1, 2, 5, 0)
    val result = program.getInstruction(0)

    result.getOrElse(new Object) mustBe a[Add]
    val add = result match {
      case Right(value) => value.asInstanceOf[Add]
      case _ => fail() // added to avoid compiler warnings
    }

    assertResult(add.readAddr1)(Parameter(2, Position))
    assertResult(add.readAddr2)(Parameter(5, Position))
    assertResult(add.storeAddr)(Parameter(0, Position))
  }

  "getInstruction(4) invoked on Program(1, 0, 0, 0, 2, 1, 3, 0)" must "return a Multiply instruction." in {
    val program = Program(1, 0, 0, 0, 2, 1, 3, 0)
    val result = program.getInstruction(4)

    result.getOrElse(new Object) mustBe a[Multiply]
    val multiply = result match {
      case Right(value) => value.asInstanceOf[Multiply]
      case _ => fail() // added to avoid compiler warnings
    }

    assertResult(multiply.readAddr1)(Parameter(1, Position))
    assertResult(multiply.readAddr2)(Parameter(3, Position))
    assertResult(multiply.storeAddr)(Parameter(0, Position))
  }

  "getInstruction(4) invoked on Program(1, 0, 0, 0, 99)" must "return a Terminate instruction." in {
    val program = Program(1, 0, 0, 0, 99)
    val result = program.getInstruction(4)

    result.getOrElse(new Object) mustBe a[Terminate]
  }

  "getInstruction(0) invoked on Program(3, 0, 99)" must "return a StoreInput instruction." in {
    val program = Program(3, 0, 99)
    val result = program.getInstruction(0)

    result.getOrElse(new Object) mustBe a[StoreInput]
    val storeInput = result match {
      case Right(value) => value.asInstanceOf[StoreInput]
      case _ => fail() // added to avoid compiler warnings
    }

    assertResult(storeInput.storeAddr)(Parameter(0, Position))
  }

  "getInstruction(0) invoked on Program(Integer.MIN_VALUE)" must "return an InvalidOpcodeError." in {
    val program = Program(Integer.MIN_VALUE)
    program.getInstruction(0).swap.getOrElse(new Object) mustBe a[InvalidOpcodeError]
  }

  "getInstruction(0) invoked on Program(1, 0, 0)" must "return an UnexpectedEndOfInstructionError." in {
    val program = Program(1, 0, 0)
    val value = program.getInstruction(0)
    value.swap.getOrElse(new Object) mustBe a[UnexpectedEndOfInstructionError]
  }

  "getInstruction(-1)" must "produce an IllegalArgumentException." in {
    assertThrows[IllegalArgumentException] {
      Program(99).getInstruction(-1)
    }
  }

  "getInstruction(1) invoked on Program(99)" must "produce an IllegalArgumentException." in {
    assertThrows[IllegalArgumentException] {
      Program(99).getInstruction(1)
    }
  }

  "applyInstruction(Add(Parameter(1), Parameter(2), Parameter(0))) invoked on Program(1, 1, 3, 0, 2, 3, 4, 8, 99)" must "return Program(4, 1, 3, 0, 2, 3, 4, 8, 99)." in {
    val program = Program(1, 1, 3, 0, 2, 3, 4, 8, 99)
    val instruction = Add(Parameter(1), Parameter(2), Parameter(0))

    val expectedProgram = Program(4, 1, 3, 0, 2, 3, 4, 8, 99)

    program.applyInstruction(instruction) match {
      case (actualProgram, actualOutput) =>
        assertResult(expectedProgram)(actualProgram)
        assert(actualOutput.isEmpty)
    }
  }

  "applyInstruction(Multiply(Parameter(2), Parameter(4), Parameter(8))) invoked on Program(1, 1, 3, 0, 2, 3, 4, 8, 99)" must "return Program(1, 1, 3, 0, 2, 3, 4, 8, 6)." in {
    val program = Program(1, 1, 3, 0, 2, 3, 4, 8, 99)
    val instruction = Multiply(Parameter(2), Parameter(4), Parameter(8))

    val expectedProgram = Program(1, 1, 3, 0, 2, 3, 4, 8, 6)
    program.applyInstruction(instruction) match {
      case (actualProgram, actualOutput) =>
        assertResult(expectedProgram)(actualProgram)
        assert(actualOutput.isEmpty)
    }
  }

  "applyInstruction(Terminate()) invoked on Program(1, 1, 3, 0, 2, 3, 4, 8, 99)" must "return the same program." in {
    val program = Program(1, 1, 3, 0, 2, 3, 4, 8, 99)
    val instruction = Terminate()

    program.applyInstruction(instruction) match {
      case (actualProgram, actualOutput) =>
        actualProgram must be theSameInstanceAs program
        assert(actualOutput.isEmpty)
    }
  }

  "applyInstruction(StoreInput(0)) invoked on Program(99)" must "produce an UnsupportedInstructionException." in {
    assertThrows[UnsupportedInstructionException] {
      Program(99).applyInstruction(StoreInput(Parameter(0)))
    }
  }

  "applyInstruction(null)" must "produce an IllegalArgumentException" in {
    assertThrows[IllegalArgumentException] {
      Program(99).applyInstruction(null)
    }
  }
}