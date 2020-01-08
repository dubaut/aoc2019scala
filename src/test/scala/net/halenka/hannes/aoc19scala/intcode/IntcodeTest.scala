package net.halenka.hannes.aoc19scala.intcode

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class IntcodeTest extends AnyFlatSpec with Matchers {
  "`apply(Seq)`" should "return a new `Intcode` instance." in {
    Intcode.apply(Seq(99)) shouldBe a[Intcode]
  }

  it should "produce an `IllegalArgumentException` if `program` is either <null> or an empty `Seq`." in {
    assertThrows[IllegalArgumentException](Intcode.apply(null))

    assertThrows[IllegalArgumentException](Intcode.apply(Nil))
  }

  "`processInstruction(Int, Seq[Int])`" should "return the modified memory after processing." in {
    assertResult(Seq(2, 0, 0, 0, 99))(Intcode.processInstruction(Seq(1, 0, 0, 0, 99), 0))
    assertResult(Seq(2, 3, 0, 6, 99))(Intcode.processInstruction(Seq(2, 3, 0, 3, 99), 0))
    assertResult(Seq(2, 4, 4, 5, 99, 9801))(Intcode.processInstruction(Seq(2, 4, 4, 5, 99, 0), 0))
    assertResult(Seq(30, 1, 1, 4, 2, 5, 6, 0, 99))(Intcode.processInstruction(Seq(1, 1, 1, 4, 99, 5, 6, 0, 99), 0))
    assertResult(Seq(3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50))(Intcode.processInstruction(Seq(1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50), 0))
  }

  it should "produce an `IllegalArgumentException` if `memory` is `null` or an empty `Seq`." in {
    assertThrows[IllegalArgumentException] {
      Intcode.processInstruction(null, 0)
    }

    assertThrows[IllegalArgumentException] {
      Intcode.processInstruction(Nil, 0)
    }
  }

  it should "produce an `IllegalArgumentException` if `address` is negative or greater than `memory.size - 1`." in {
    val seq = Seq(0, 1, 2, 3)

    assertThrows[IllegalArgumentException] {

      Intcode.processInstruction(seq, -1)
    }

    assertThrows[IllegalArgumentException] {
      Intcode.processInstruction(seq, seq.size)
    }
  }
}