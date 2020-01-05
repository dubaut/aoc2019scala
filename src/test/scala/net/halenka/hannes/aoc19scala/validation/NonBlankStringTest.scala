package net.halenka.hannes.aoc19scala.validation

import org.apache.commons.lang3.StringUtils
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class NonBlankStringTest extends AnyFlatSpec with Matchers {
  "`apply(String)`" should "produce an IllegalArgumentException if the provided string value is blank." in {
    assertThrows[IllegalArgumentException] {
      NonBlankString(null)
    }

    assertThrows[IllegalArgumentException] {
      NonBlankString(StringUtils.EMPTY)
    }

    assertThrows[IllegalArgumentException] {
      NonBlankString(" \n")
    }
  }

  "`value`" should "provide the wrapped string value." in {
    val expected = "Some string value."
    val nbs = NonBlankString(expected)
    val actual = nbs.value

    assertResult(expected)(actual)
  }
}