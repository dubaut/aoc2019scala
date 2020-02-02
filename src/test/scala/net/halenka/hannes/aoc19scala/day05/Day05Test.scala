package net.halenka.hannes.aoc19scala.day05

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class Day05Test extends AnyFlatSpec with Matchers {
  "Day05.answer" must "be Day05Result(\"14522484\")." in {
    val expected = Day05Result("14522484")
    val result = Day05.answer

    result mustBe a[Right[_, Day05Result]]
    result match {
      case Right(actual) =>
        assertResult(expected)(actual)
      case _ => fail()
    }
  }
}
