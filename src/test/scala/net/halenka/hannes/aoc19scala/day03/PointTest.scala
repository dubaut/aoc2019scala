package net.halenka.hannes.aoc19scala.day03

import org.scalatest.flatspec.AnyFlatSpec

class PointTest extends AnyFlatSpec {
  "`manhattanDistance(Point)`" should "return `12` if invoked on `Point(0, 0)` for `Point(6, 6)`" in {
    val expected = 12
    val actual = Point(0, 0).manhattanDistance(Point(6, 6))

    assertResult(expected)(actual)
  }

  it should "return `12` for `Point(-3, -3)` and `Point(3, 3)`." in {
    val expected = 12
    val actual = Point(-3, -3).manhattanDistance(Point(3, 3))

    assertResult(expected)(actual)
  }

  it should "return `7` for `Point(-3, 3)` and `Point(0, -1)`." in {
    val expected = 7
    val actual = Point(-3, 3).manhattanDistance(Point(0, -1))

    assertResult(expected)(actual)
  }

  it should "produce a `NullPointerException` if `that` is `null`." in {
    assertThrows[IllegalArgumentException] {
      Point(0, 0).manhattanDistance(null)
    }
  }

  "`closestManhattanDistance(Point, Point)`" should "return the closest point and its Manhattan Distance." in {
    val point = Point(0, 0)

    val expected1 = (Point(1, 1), 2)
    val actual1 = point.closestManhattanDistance(Point(1, 1), Point(2, 2))
    assertResult(expected1)(actual1)

    val expected2 = (Point(-1, -1), 2)
    val actual2 = point.closestManhattanDistance(Point(2, 2), Point(-1, -1))
    assertResult(expected2)(actual2)

    val expected3 = (Point(0, 0), 0)
    val actual3 = point.closestManhattanDistance(Point(-3, -1), Point(0, 0))
    assertResult(expected3)(actual3)

    val expected4 = (Point(1, 1), 2)
    val actual4 = point.closestManhattanDistance(Point(1, 1), Point(-1, -1))
    assertResult(expected4)(actual4)

    val expected5 = (Point(0, -2), 2)
    val actual5 = point.closestManhattanDistance(Point(0, -2), Point(0, 2))
    assertResult(expected5)(actual5)
  }

  it should "produce an `IllegalArgumentException` if either `point1` or `point2` is `null`.`" in {
    val point = Point(0, 0)

    assertThrows[IllegalArgumentException] {
      point.closestManhattanDistance(null, Point(0, 0))
    }

    assertThrows[IllegalArgumentException] {
      point.closestManhattanDistance(Point(0, 0), null)
    }
  }
}