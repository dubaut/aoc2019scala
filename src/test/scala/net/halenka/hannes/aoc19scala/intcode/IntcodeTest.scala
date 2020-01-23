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

  "`processInstruction(Int, Seq[Int])`" should "return the modified memory after processing." in {
    assertResult(Seq(2, 0, 0, 0, 99))(processInstruction(Seq(1, 0, 0, 0, 99), 0))
    assertResult(Seq(2, 3, 0, 6, 99))(processInstruction(Seq(2, 3, 0, 3, 99), 0))
    assertResult(Seq(2, 4, 4, 5, 99, 9801))(processInstruction(Seq(2, 4, 4, 5, 99, 0), 0))
    assertResult(Seq(30, 1, 1, 4, 2, 5, 6, 0, 99))(processInstruction(Seq(1, 1, 1, 4, 99, 5, 6, 0, 99), 0))
    assertResult(Seq(3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50))(processInstruction(Seq(1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50), 0))
  }

  it should "produce an `IllegalArgumentException` if `memory` is `null` or an empty `Seq`." in {
    assertThrows[IllegalArgumentException] {
      processInstruction(null, 0)
    }

    assertThrows[IllegalArgumentException] {
      processInstruction(Nil, 0)
    }
  }

  it should "produce an `IllegalArgumentException` if `address` is negative or greater than `memory.size - 1`." in {
    val seq = Seq(0, 1, 2, 3)

    assertThrows[IllegalArgumentException] {
      processInstruction(seq, -1)
    }

    assertThrows[IllegalArgumentException] {
      Intcode.processInstruction(seq, seq.size)
    }
  }

  "`loadInstruction(IndexedSeq[Int], Int`)" must "return a `Right` containing an `Add` if the opcode of the instruction is '1." in {
    loadInstruction(IndexedSeq(1, 2, 5, 0), 0).getOrElse(null) mustBe a[Add]
  }

  it must "return a `Multiply` if the opcode of the instruction is '2'." in {
    loadInstruction(IndexedSeq(1, 0, 0, 0, 2, 1, 3, 0), 4).getOrElse(null) mustBe a[Multiply]
  }

  it must "return a `Terminate` if the opcode of the instruction is '99'." in {
    loadInstruction(IndexedSeq(1, 0, 0, 0, 99), 4).getOrElse(null) mustBe a[Terminate]
  }

  it must "produce an `InvalidOpcodeError` if the opcode of the instruction is unknown." in {
    loadInstruction(IndexedSeq(-1), 0).swap.getOrElse(null) mustBe a[InvalidOpcodeError]
  }

  it must "produce an `UnexpectedEndOfInstructionError` if the instruction does not have a sufficient number of parameter." in {
    loadInstruction(IndexedSeq(1, 0, 0), 0).swap.getOrElse(null) mustBe a[UnexpectedEndOfInstructionError]
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

  "`applyInstruction(Instruction, IndexedSeq[Int])`" must "return the effected program and its output." in {
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

  "`run(IndexedSeq[Int])`" must "return the modified program and the output after processing all instructions." in {
    assertResult((IndexedSeq(2, 0, 0, 0, 99), IndexedSeq[Int]()))(Intcode.run(IndexedSeq(1, 0, 0, 0, 99)))
    assertResult((IndexedSeq(2, 3, 0, 6, 99), IndexedSeq[Int]()))(Intcode.run(IndexedSeq(2, 3, 0, 3, 99)))
    assertResult((IndexedSeq(2, 4, 4, 5, 99, 9801), IndexedSeq[Int]()))(Intcode.run(IndexedSeq(2, 4, 4, 5, 99, 0)))
    assertResult((IndexedSeq(30, 1, 1, 4, 2, 5, 6, 0, 99), IndexedSeq[Int]()))(Intcode.run(IndexedSeq(1, 1, 1, 4, 99, 5, 6, 0, 99)))
    assertResult((IndexedSeq(3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50), IndexedSeq[Int]()))(Intcode.run(IndexedSeq(1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50)))
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