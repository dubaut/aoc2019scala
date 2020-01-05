package net.halenka.hannes.aoc19scala

import org.apache.commons.lang3.StringUtils
import org.scalatest.TryValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class aoc19scalaTest extends AnyFlatSpec with Matchers with TryValues {
  "`loadTextFileResource(String)`" should "return a sequence containing all lines of the resource." in {
    val result = loadTextFileResource("loadTextFileResource.txt")
    result should be a Symbol("success")
    assert(result.success.value == Seq("line 1", "line 2", "line 3"))
  }

  it should "return an empty sequence if the file is empty." in {
    val result = loadTextFileResource("empty.txt")
    result should be a Symbol("success")
    assert(result.success.value == Nil)
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
}