package net.halenka.hannes.aoc19scala.day04

import net.halenka.hannes.aoc19scala.day04.Day04._
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day04Test extends AnyFlatSpec with EitherValues with Matchers{
  "`answer`" should "should contain `Day04Result(1246)`." in {
    assertResult(true)(answer.contains(Day04Result(1246)))
  }

  "`calculatePart1(Range)`" should "return a sequence of 1246 elements for the range `234208 to 765869`." in {
    val part1 = calculatePart1(234208 to 765869)

    part1 shouldBe a[IndexedSeq[Int]]
    part1 should have size 1246
  }

  it should "produce an `IllegalArgumentException` if `range` is `null`." in {
    assertThrows[IllegalArgumentException] {
      calculatePart1(null)
    }
  }

  "`hasEqualAdjacentDigits(Int)`" should "return `true` for numbers with two or more equal adjacent digits." in {
    assertResult(true)(hasEqualAdjacentDigits(5331))
    assertResult(true)(hasEqualAdjacentDigits(53377))
    assertResult(true)(hasEqualAdjacentDigits(533877))
    assertResult(true)(hasEqualAdjacentDigits(33))
    assertResult(true)(hasEqualAdjacentDigits(53331))
  }

  it should "return `false` for numbers without equal adjacent digits." in {
    assertResult(false)(hasEqualAdjacentDigits(1))
    assertResult(false)(hasEqualAdjacentDigits(123))
  }

  "`digitsNeverDecrease(Int)`" should "return `true` for numbers whose digits never decrease." in {
    assertResult(true)(digitsNeverDecrease(1))
    assertResult(true)(digitsNeverDecrease(22))
    assertResult(true)(digitsNeverDecrease(123))
  }

  it should "return `false` for numbers whose digits decrease." in {
    assertResult(false)(digitsNeverDecrease(321))
  }
 }
