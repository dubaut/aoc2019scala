package net.halenka.hannes.aoc19scala.validation

import org.apache.commons.lang3.StringUtils
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StringValidatorTest extends AnyFlatSpec with Matchers {
  "`requireNonBlank(String)`" should "return a `NonBlankString`." in {
    val expected = "Some value."
    val actual = StringValidator(expected).requireNonBlank(StringUtils.EMPTY).value
    assertResult(expected)(actual)
  }

  it should "produce a `IllegalArgumentException` if `string` is blank." in {
    assertThrows[IllegalArgumentException] {
      StringValidator(null).requireNonBlank(StringUtils.EMPTY)
    }

    assertThrows[IllegalArgumentException] {
      StringValidator(StringUtils.EMPTY).requireNonBlank(StringUtils.EMPTY)
    }

    assertThrows[IllegalArgumentException] {
      StringValidator(" \n").requireNonBlank(StringUtils.EMPTY)
    }
  }

  it should "use `message` as exception message if an exception is produced." in {
    val expected = "Exception message."
    val caught = intercept[IllegalArgumentException] {
      StringValidator(null).requireNonBlank(expected)
    }
    val actual = caught.getMessage

    assertResult(expected)(actual)
  }
}