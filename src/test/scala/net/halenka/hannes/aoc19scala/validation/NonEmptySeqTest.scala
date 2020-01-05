package net.halenka.hannes.aoc19scala.validation

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class NonEmptySeqTest extends AnyFunSuite with Matchers {
  test("`apply(Seq)` should create a new `NonEmptySeq` when the specified `Seq` is neither <null> nor empty.") {
    NonEmptySeq.apply(Seq(1, 2, 3)) shouldBe a[NonEmptySeq[Int]]
  }

  test("`value` should provide the exact same object as used to create `NonEmptySeq` instance.") {
    val seq = Seq(1, 2, 3)
    val nes = NonEmptySeq(seq)
    nes.value should be theSameInstanceAs seq
  }

  test("`apply(Seq)` should produce an `IllegalArgumentException` if the specified `Seq` is either <null> or empty.") {
    assertThrows[IllegalArgumentException] {
      NonEmptySeq.apply(null)
    }

    assertThrows[IllegalArgumentException] {
      NonEmptySeq.apply(Nil)
    }
  }

  test("`nonEmptySeq2Seq(NonEmptySeq)` should return the wrapped object of the provided `NonEmptySeq`.") {
    val seq = Seq(1, 2, 3)
    NonEmptySeq.nonEmptySeq2Seq(NonEmptySeq(seq)) should be theSameInstanceAs seq
  }
}