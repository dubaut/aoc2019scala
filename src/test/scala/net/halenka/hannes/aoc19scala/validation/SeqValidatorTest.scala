package net.halenka.hannes.aoc19scala.validation

import org.apache.commons.lang3.StringUtils
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class SeqValidatorTest extends AnyFunSuite with Matchers {
  test("`requireNonEmpty(String)` should return a `NonEmptySeq` when `seq` is neither <null> nor empty.") {
    val seq = Seq(1, 2, 3)
    val nes = SeqValidator(seq).requireNonEmpty(StringUtils.EMPTY)

    nes shouldBe a[NonEmptySeq[_]]
  }

  test("`requireNonEmpty(String)` should produce an `IllegalArgumentException` if `seq` is either <null> or an empty `Seq`.") {
    assertThrows[IllegalArgumentException] {
      SeqValidator(null).requireNonEmpty(StringUtils.EMPTY)
    }

    assertThrows[IllegalArgumentException] {
      SeqValidator(Nil).requireNonEmpty(StringUtils.EMPTY)
    }
  }

  test("Exceptions produced by `requireNonEmpty(String)` should have the message specified in `message`.") {
    val message = "Some message."

    val caught = intercept[IllegalArgumentException] {
      SeqValidator(Nil).requireNonEmpty(message)
    }

    assertResult(message)(caught.getMessage)
  }
}