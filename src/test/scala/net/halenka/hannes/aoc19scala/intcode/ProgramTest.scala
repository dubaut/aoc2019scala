package net.halenka.hannes.aoc19scala.intcode

import org.scalatest.flatspec.AnyFlatSpec

class ProgramTest extends AnyFlatSpec {
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
}
