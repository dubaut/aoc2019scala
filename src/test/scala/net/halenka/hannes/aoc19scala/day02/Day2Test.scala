package net.halenka.hannes.aoc19scala.day02

import net.halenka.hannes.aoc19scala.day02.Day02._
import org.scalatest.TryValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day2Test extends AnyFlatSpec with TryValues with Matchers {
  "`loadResourceAsIntSeq(String)`" should "return a `Success`." in {
    val result = loadResourceAsIntSeq("day02/test_input.txt")
    result should be a Symbol("Success")
    assert(result.success.value == Seq(1, 19, 2, 19, 6, 23, 2, 13, 23, 27, 1, 9, 27, 31, 2, 31))
  }

  it should "return an empty sequence if the input is empty." in {
    val result = loadResourceAsIntSeq("empty.txt")
    result should be a Symbol("Success")
    assert(result.success.value == Seq())
  }

  it should "produce an `IllegalArgumentException` if `resource` blank." in {
    assertThrows[IllegalArgumentException] {
      loadResourceAsIntSeq("   ")
    }
  }

  it should "return a `Failure` if the specified resource does not exist." in {
    val result = loadResourceAsIntSeq("does/not/exist")
    result should be a Symbol("failure")
  }

  it should "return a `Failure` if the resource cannot be parsed." in {
    val result = loadResourceAsIntSeq("day02/not_parsable_input.txt")
    result should be a Symbol("failure")
  }
}