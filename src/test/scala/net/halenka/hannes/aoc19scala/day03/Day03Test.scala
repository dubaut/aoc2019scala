package net.halenka.hannes.aoc19scala.day03

import net.halenka.hannes.aoc19scala.validation.NonBlankString
import org.apache.commons.lang3.StringUtils
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day03Test extends AnyFlatSpec with Matchers with OptionValues {
  "`Day03.calculateShortestDistance(String, String)`" should "produce an `IllegalArgumentException` if either or both of the parameters is blank." in {
    assertThrows[IllegalArgumentException] {
      Day03.calculateShortestDistance(null, "R1")
    }

    assertThrows[IllegalArgumentException] {
      Day03.calculateShortestDistance(StringUtils.EMPTY, "R1")
    }

    assertThrows[IllegalArgumentException] {
      Day03.calculateShortestDistance(" \n\t", "R1")
    }

    assertThrows[IllegalArgumentException] {
      Day03.calculateShortestDistance("R1", null)
    }

    assertThrows[IllegalArgumentException] {
      Day03.calculateShortestDistance("R1", StringUtils.EMPTY)
    }

    assertThrows[IllegalArgumentException] {
      Day03.calculateShortestDistance("R1", " \n\t")
    }
  }

  "The distance for 'R75,D30,R83,U83,L12,D49,R71,U7,L72' and 'U62,R66,U55,R34,D71,R55,D58,R83'" should "be 159." in {
    val path1 = "R75,D30,R83,U83,L12,D49,R71,U7,L72"
    val path2 = "U62,R66,U55,R34,D71,R55,D58,R83"

    val actual = Day03.calculateShortestDistance(path1, path2)
    assert(actual.value == 159)
  }

  "The distance for 'R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51' and 'U98,R91,D20,R16,D67,R40,U7,R15,U6,R7'" should "be 135." in {
    val path1 = "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
    val path2 = "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"

    val actual = Day03.calculateShortestDistance(path1, path2)
    assert(actual.value == 135)
  }

  "`getWiringInstructions(NonBlankString)`" should "return a `Vector[WiringInstruction]` containing `WiringInstruction` elements based on the provided `path`." in {
    val path = "R98,U47,R26,D63"
    val expected = Vector(WiringInstruction("R98"), WiringInstruction("U47"), WiringInstruction("R26"), WiringInstruction("D63"))

    val actual = Day03.getWiringInstructions(NonBlankString(path))

    assertResult(expected)(actual)
  }

  it should "produce a `NullPointerException` if `path` is `null`." in {
    assertThrows[NullPointerException] {
      Day03.getWiringInstructions(null)
    }
  }

  "`getCoordinates(Vector[WiringInstruction])`" should "return a sequence of points." in {
    val wi = Vector(WiringInstruction("R3"), WiringInstruction("U2"), WiringInstruction("L2"), WiringInstruction("D1"))

    val expected = Vector(
      Point(0, 0), Point(1, 0), Point(2, 0), Point(3, 0),
      Point(3, 1), Point(3, 2),
      Point(2, 2), Point(1, 2),
      Point(1, 1)
    )
    val actual = Day03.getCoordinates(wi)

    assertResult(expected)(actual)
  }

  it should "produce an `IllegalArgumentException` if `wi` is `null` or empty." in {
    assertThrows[IllegalArgumentException] {
      Day03.getCoordinates(null)
    }

    assertThrows[IllegalArgumentException] {
      Day03.getCoordinates(Vector())
    }
  }

  "`getCoordinatesForInstruction(Point, WiringInstructions)`" should "return a sequence of `Point`s." in {
    assertResult(Vector(Point(1, 0), Point(2, 0), Point(3, 0)))(Day03.getCoordinatesForInstruction(Point(0, 0), WiringInstruction("R3")))

    assertResult(Vector(Point(0, 3)))(Day03.getCoordinatesForInstruction(Point(1, 3), WiringInstruction("L1")))

    assertResult(Vector(Point(1, 2), Point(1, 3)))(Day03.getCoordinatesForInstruction(Point(1, 1), WiringInstruction("U2")))

    assertResult(Vector(Point(3, 6), Point(3, 5), Point(3, 4)))(Day03.getCoordinatesForInstruction(Point(3, 7), WiringInstruction("D3")))
  }

  it should "produce a `NullPointerException` if either `offset` or `wi` is `null`." in {
    assertThrows[NullPointerException] {
      Day03.getCoordinatesForInstruction(null, null)
    }

    assertThrows[NullPointerException] {
      Day03.getCoordinatesForInstruction(Point(0, 0), null)
    }

    assertThrows[NullPointerException] {
      Day03.getCoordinatesForInstruction(null, WiringInstruction("L1"))
    }
  }
}