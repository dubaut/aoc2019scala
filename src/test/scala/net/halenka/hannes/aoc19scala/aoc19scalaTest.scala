package net.halenka.hannes.aoc19scala

import org.apache.commons.lang3.StringUtils
import org.scalatest.TryValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class aoc19scalaTest extends AnyFlatSpec with Matchers with TryValues {
  "`loadTextFileResource(String)`" should "return a sequence containing all lines of the resource." in {
    val result = loadTextFileResource("loadTextFileResource.txt")
    result should be a Symbol("success")
    assert(result.success.value == Vector("line 1", "line 2", "line 3"))
  }

  it should "return an empty sequence if the file is empty." in {
    val result = loadTextFileResource("empty.txt")
    result should be a Symbol("success")
    assert(result.success.value == Vector())
  }

  it should "produce an `IllegalArgumentException` if the `resource` is `null`, empty or blank." in {
    assertThrows[IllegalArgumentException] {
      loadTextFileResource(null)
    }

    assertThrows[IllegalArgumentException] {
      loadTextFileResource(StringUtils.EMPTY)
    }

    assertThrows[IllegalArgumentException] {
      loadTextFileResource("  " + '\n')
    }
  }

  it should "return a `Failure` if the specified resource does not exist or cannot be read." in {
    val result = loadTextFileResource("resource/does/not/exist")
    result should be a Symbol("failure")
  }

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