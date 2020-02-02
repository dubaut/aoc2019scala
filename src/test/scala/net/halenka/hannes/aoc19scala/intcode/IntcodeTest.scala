package net.halenka.hannes.aoc19scala.intcode

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.{OptionValues, TryValues}

class IntcodeTest extends AnyFlatSpec with Matchers with TryValues with OptionValues {
  "Intcode(Program(99))" must "return a new Intcode instance." in {
    val program = Program(99)
    val intcode = Intcode(program)

    intcode mustBe a[Intcode]
    intcode.program must be theSameInstanceAs program
  }

  "Intcode(null)" must "produce an IllegalArgumentException." in {
    assertThrows[IllegalArgumentException](Intcode(null))
  }

  "Intcode.run(Program(1, 0, 0, 0, 99))" must "return IndexedSeq(2, 0, 0, 0, 99) and an empty output." in {
    val program = Program(1, 0, 0, 0, 99)

    val expectedMemory = IndexedSeq(2, 0, 0, 0, 99)
    val expectedOutput = Nil.toIndexedSeq

    Intcode.run(program) match {
      case (actualMemory, actualOutput) =>
        assertResult(expectedMemory)(actualMemory)
        assertResult(expectedOutput)(actualOutput)
    }
  }

  "Intcode.run(Program(2, 3, 0, 3, 99))" must "return IndexedSeq(2, 3, 0, 6, 99) and an empty output." in {
    val program = Program(2, 3, 0, 3, 99)

    val expectedMemory = IndexedSeq(2, 3, 0, 6, 99)
    val expectedOutput = Nil.toIndexedSeq

    Intcode.run(program) match {
      case (actualMemory, actualOutput) =>
        assertResult(expectedMemory)(actualMemory)
        assertResult(expectedOutput)(actualOutput)
    }
  }

  "Intcode.run(Program(2, 4, 4, 5, 99, 0))" must "return IndexedSeq(2, 4, 4, 5, 99, 9801) and an empty output." in {
    val program = Program(2, 4, 4, 5, 99, 0)

    val expectedMemory = IndexedSeq(2, 4, 4, 5, 99, 9801)
    val expectedOutput = Nil.toIndexedSeq

    Intcode.run(program) match {
      case (actualMemory, actualOutput) =>
        assertResult(expectedMemory)(actualMemory)
        assertResult(expectedOutput)(actualOutput)
    }
  }

  "Intcode.run(Program(1, 1, 1, 4, 99, 5, 6, 0, 99))" must "return IndexedSeq(30, 1, 1, 4, 2, 5, 6, 0, 99) and an empty output." in {
    val program = Program(1, 1, 1, 4, 99, 5, 6, 0, 99)

    val expectedMemory = IndexedSeq(30, 1, 1, 4, 2, 5, 6, 0, 99)
    val expectedOutput = Nil.toIndexedSeq

    Intcode.run(program) match {
      case (actualMemory, actualOutput) =>
        assertResult(expectedMemory)(actualMemory)
        assertResult(expectedOutput)(actualOutput)
    }
  }

  "Intcode.run(Program(1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50))" must "return IndexedSeq(3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50) and an empty output." in {
    val program = Program(1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50)

    val expectedMemory = IndexedSeq(3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50)
    val expectedOutput = Nil.toIndexedSeq

    Intcode.run(program) match {
      case (actualMemory, actualOutput) =>
        assertResult(expectedMemory)(actualMemory)
        assertResult(expectedOutput)(actualOutput)
    }
  }

  "Intcode.run(Program(3, 0, 3, 1, 99))" must "return IndexedSeq(1, 1, 3, 1, 99) and an empty output." in {
    val program = Program(3, 0, 3, 1, 99)

    val expectedMemory = IndexedSeq(1, 1, 3, 1, 99)
    val expectedOutput = Nil.toIndexedSeq

    Intcode.run(program) match {
      case (actualMemory, actualOutput) =>
        assertResult(expectedMemory)(actualMemory)
        assertResult(expectedOutput)(actualOutput)
    }
  }

  "Intcode.run(null)" must "produce an IllegalArgumentException." in {
    assertThrows[IllegalArgumentException](Intcode.run(null))
  }

  "Intcode.run(Program(99), -1)" must "produce an IllegalArgumentException." in {
    assertThrows[IllegalArgumentException](Intcode.run(Program(99), -1))
  }

  "Intcode.run(Program(99), 0, null)" must "produce an IllegalArgumentException." in {
    assertThrows[IllegalArgumentException](Intcode.run(Program(99), 0, null))
  }

  "Intcode.run(Program(4, 0, 99))" must "return memory IndexedSeq(4, 0, 99) and output IndexedSeq(4)." in {
    val program = Program(4, 0, 99)

    val expectedMemory = IndexedSeq(4, 0, 99)
    val expectedOutput = IndexedSeq(4)

    Intcode.run(program) match {
      case (actualProgram, actualOutput) =>
        assertResult(expectedMemory)(actualProgram)
        assertResult(expectedOutput)(actualOutput)
    }
  }
}