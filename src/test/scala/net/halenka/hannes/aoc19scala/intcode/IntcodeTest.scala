package net.halenka.hannes.aoc19scala.intcode

import net.halenka.hannes.aoc19scala.intcode.Intcode._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.{OptionValues, TryValues}

class IntcodeTest extends AnyFlatSpec with Matchers with TryValues with OptionValues {
  "`Intcode(Seq)`" should "return a new `Intcode` instance." in {
    Intcode(IndexedSeq(99)) mustBe a[Intcode]
  }

  it should "produce an `IllegalArgumentException` if `program` is either <null> or an empty `Seq`." in {
    assertThrows[IllegalArgumentException](Intcode(null))

    assertThrows[IllegalArgumentException](Intcode(IndexedSeq()))
  }

  "`loadInstruction`" must "return a `Right` containing an `Add` if the opcode of the instruction is '1." in {
    loadInstruction(IndexedSeq(1, 2, 5, 0), 0).getOrElse(new Object) mustBe a[Add]
  }

  it must "return a `Multiply` if the opcode of the instruction is '2'." in {
    loadInstruction(IndexedSeq(1, 0, 0, 0, 2, 1, 3, 0), 4).getOrElse(new Object) mustBe a[Multiply]
  }

  it must "return a `Terminate` if the opcode of the instruction is '99'." in {
    loadInstruction(IndexedSeq(1, 0, 0, 0, 99), 4).getOrElse(new Object) mustBe a[Terminate]
  }

  it must "return a ´StoreInput´ if the opcode of the instruction is '3'." in {
    loadInstruction(IndexedSeq(3, 0, 99), 0).getOrElse(new Object) mustBe a[StoreInput]
  }

  it must "produce an `InvalidOpcodeError` if the opcode of the instruction is unknown." in {
    loadInstruction(IndexedSeq(-1), 0).swap.getOrElse(new Object) mustBe a[InvalidOpcodeError]
  }

  it must "produce an `UnexpectedEndOfInstructionError` if the instruction does not have a sufficient number of parameter." in {
    loadInstruction(IndexedSeq(1, 0, 0), 0).swap.getOrElse(new Object) mustBe a[UnexpectedEndOfInstructionError]
  }

  it must "produce an `IllegalArgumentException` if `program` is `null`." in {
    assertThrows[IllegalArgumentException] {
      loadInstruction(null, 0)
    }
  }

  it must "produce an `IllegalArgumentException` if `program` is empty." in {
    assertThrows[IllegalArgumentException] {
      loadInstruction(IndexedSeq(), 0)
    }
  }

  it must "produce an `IllegalArgumentException` if `address` is `< 0`." in {
    assertThrows[IllegalArgumentException] {
      loadInstruction(IndexedSeq(99), -1)
    }
  }

  it must "produce an `IllegalArgumentException` if `address` is >= `program.size`." in {
    val program = IndexedSeq(99)

    assertThrows[IllegalArgumentException] {
      loadInstruction(program, program.size)
    }

    assertThrows[IllegalArgumentException] {
      loadInstruction(program, program.size + 1)
    }
  }

  "`applyInstruction(..)`" must "return the effected program and its output." in {
    val program = IndexedSeq(1, 1, 3, 0, 2, 3, 4, 8, 99)

    applyInstruction(Add(1, 2, 0), program) match {
      case (actualProgram, actualOutput) =>
        val expectedProgram = IndexedSeq(4, 1, 3, 0, 2, 3, 4, 8, 99)
        assertResult(expectedProgram)(actualProgram)
        assert(actualOutput.isEmpty)
    }

    applyInstruction(Multiply(2, 4, 8), program) match {
      case (actualProgram, actualOutput) =>
        val expectedProgram = IndexedSeq(1, 1, 3, 0, 2, 3, 4, 8, 6)
        assertResult(expectedProgram)(actualProgram)
        assert(actualOutput.isEmpty)
    }

    applyInstruction(Terminate(), program) match {
      case (actualProgram, actualOutput) =>
        val expectedProgram = program
        assertResult(expectedProgram)(actualProgram)
        assert(actualOutput.isEmpty)
    }
  }

  it must "produce an `IllegalArgumentException` if `instruction` is `null`." in {
    assertThrows[IllegalArgumentException] {
      applyInstruction(null, IndexedSeq(99))
    }
  }

  it must "produce an `IllegalArgumentException` if `program` is either `null` or empty." in {
    assertThrows[IllegalArgumentException] {
      applyInstruction(Terminate(), null)
    }

    assertThrows[IllegalArgumentException] {
      applyInstruction(Terminate(), IndexedSeq[Int]())
    }
  }

  "`applyInstructionWithInput(..)`" must "return the effected program and its output." in {
    val instruction = StoreInput(0)
    val program = IndexedSeq(3, 0, 99)
    val input = 1

    val expected = (IndexedSeq(1, 0, 99), None)
    val actual = applyInstructionWithInput(instruction, program, input)
    assertResult(expected)(actual)
  }

  it must "produce an `IllegalArgumentException` if `instruction` `null`" in {
    assertThrows[IllegalArgumentException] {
      applyInstructionWithInput(null, IndexedSeq(99), 0)
    }
  }

  it must "produce an `IllegalArgumentException` if `program` is either `null` or empty." in {
    assertThrows[IllegalArgumentException] {
      applyInstructionWithInput(StoreInput(0), null, 0)
    }

    assertThrows[IllegalArgumentException] {
      applyInstructionWithInput(StoreInput(0), Nil.toIndexedSeq, 0)
    }
  }

  "`run(..)`" must "return the modified program and the output after processing all instructions." in {
    val emptyOutput = Nil.toIndexedSeq

    assertResult((IndexedSeq(2, 0, 0, 0, 99), emptyOutput))(Intcode.run(IndexedSeq(1, 0, 0, 0, 99)))
    assertResult((IndexedSeq(2, 3, 0, 6, 99), emptyOutput))(Intcode.run(IndexedSeq(2, 3, 0, 3, 99)))
    assertResult((IndexedSeq(2, 4, 4, 5, 99, 9801), emptyOutput))(Intcode.run(IndexedSeq(2, 4, 4, 5, 99, 0)))
    assertResult((IndexedSeq(30, 1, 1, 4, 2, 5, 6, 0, 99), emptyOutput))(Intcode.run(IndexedSeq(1, 1, 1, 4, 99, 5, 6, 0, 99)))
    assertResult((IndexedSeq(3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50), emptyOutput))(Intcode.run(IndexedSeq(1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50)))

    assertResult((IndexedSeq(1, 1, 3, 1, 99), emptyOutput))(Intcode.run(IndexedSeq(3, 0, 3, 1, 99)))
    //assertResult(IndexedSeq(1002, 4, 3, 4, 99), emptyOutput)(Intcode.run(IndexedSeq(1002, 4, 3, 4, 33)))
  }

  it must "produce an `IllegalArgumentException` if `program` is `null` or empty." in {
    assertThrows[IllegalArgumentException] {
      Intcode.run(null)
    }

    assertThrows[IllegalArgumentException] {
      Intcode.run(IndexedSeq())
    }
  }

  it must "produce an `IllegalArgumentException` if `address` is less than '0'." in {
    assertThrows[IllegalArgumentException] {
      Intcode.run(IndexedSeq(99), -1)
    }
  }
}