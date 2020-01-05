package net.halenka.hannes.aoc19scala

import net.halenka.hannes.aoc19scala.Day02._
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

  "`runIntcode(Seq)`" should "return <2,0,0,0,99> for <1,0,0,0,99>" in {
    val expected = Seq(2, 0, 0, 0, 99)
    val actual = runIntcode(Seq(1, 0, 0, 0, 99))
    assertResult(expected)(actual)
  }

  it should "return <2,3,0,6,99> for <2,3,0,3,99>" in {
    val expected = Seq(2, 3, 0, 6, 99)
    val actual = runIntcode(Seq(2, 3, 0, 3, 99))
    assertResult(expected)(actual)
  }

  it should "return <2,4,4,5,99,9801> for <2,4,4,5,99,0>" in {
    val expected = Seq(2, 4, 4, 5, 99, 9801)
    val actual = runIntcode(Seq(2, 4, 4, 5, 99, 0))
    assertResult(expected)(actual)
  }

  it should "return <30,1,1,4,2,5,6,0,99> for <1,1,1,4,99,5,6,0,99>" in {
    val expected = Seq(30, 1, 1, 4, 2, 5, 6, 0, 99)
    val actual = runIntcode(Seq(1, 1, 1, 4, 99, 5, 6, 0, 99))
    assertResult(expected)(actual)
  }

  it should "return <3500,9,10,70,2,3,11,0,99,30,40,50> for <1,9,10,3,2,3,11,0,99,30,40,50>" in {
    val expected = Seq(3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50)
    val actual = runIntcode(Seq(1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50))
    assertResult(expected)(actual)
  }

  it should "return a `IllegalArgumentException` if  `program` is <null> or empty." in {
    assertThrows[IllegalArgumentException] {
      runIntcode(null)
    }
    assertThrows[IllegalArgumentException] {
      runIntcode(Seq())
    }
  }
}