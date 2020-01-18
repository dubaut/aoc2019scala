package net.halenka.hannes.aoc19scala.day03

import net.halenka.hannes.aoc19scala.loadTextFileResource
import net.halenka.hannes.aoc19scala.validation.{NonBlankString, SeqValidator, StringValidator}

import scala.util.{Failure, Success}

object Day03 {
  private final val Origin: Point = Point(0, 0)

  def answer: Either[Any, Day03Result] = {
    def calculatePart1(intersections: Vector[Point]) = {
      if (intersections.nonEmpty) {
        val closest = intersections.reduce((p1, p2) => Origin.closestManhattanDistance(p1, p2)._1)
        Right(Day03Result(Origin.manhattanDistance(closest), _))
      } else {
        Left("The wires have no intersection.")
      }
    }

    def calculatePart2(intersections: IndexedSeq[Point], path1: IndexedSeq[Point], path2: IndexedSeq[Point]): Int = {
      def getStepPos(point: Point, path: IndexedSeq[Point]): Int = {
        val pathZipped = path.zipWithIndex

        pathZipped.find(_._1 == point) match {
          case Some(step) => {
            step._2
          }
          case _ => throw new RuntimeException(s"Could not find $point in $path.")
        }
      }

      intersections.foldLeft(Int.MaxValue)((minSteps, point) => {
        val posInPath1 = getStepPos(point, path1)
        val posInPath2 = getStepPos(point, path2)

        val totalSteps = posInPath1 + posInPath2

        if (totalSteps < minSteps) totalSteps else minSteps
      })
    }

    loadTextFileResource("day03/input.txt") match {
      case Success(lines) =>
        lines.size match {
          case 2 =>
            val wi = lines.map(line => getWiringInstructions(NonBlankString(line)))

            val stepsPath1 = getCoordinates(wi(0))
            val stepsPath2 = getCoordinates(wi(1))

            val intersections = stepsPath1.intersect(stepsPath2).filter(_ != Origin)

            val part1 = calculatePart1(intersections)

            part1 match {
              case Right(result) => {
                val part2: Int = calculatePart2(intersections, stepsPath1, stepsPath2)

                Right(result(part2))
              }
              case Left(left) => Left(left)
              case _ => throw new RuntimeException("Unexpected result.")
            }
          case size: Int => Left(s"Invalid number of lines: $size")
        }
      case Failure(ex) => Left(ex)
    }
  }

  /** Returns the <i>Manhattan Distance</i> between Origin and the closes intersection between to paths.
   *
   * @throws IllegalArgumentException if either `path1` or `path2` is `null`.
   */
  def calculateShortestDistance(path1: String, path2: String): Option[Int] = {
    val _path1 = path1.requireNonBlank("`path1` must not be blank.")
    val _path2 = path2.requireNonBlank("`path2` must not be blank.")

    val wiringInstructions = Vector(getWiringInstructions(_path1), getWiringInstructions(_path2))
    val coordinates = wiringInstructions.map(getCoordinates)
    val intersections = coordinates.reduceLeft(_.intersect(_)).filter(_ != Origin)

    if (intersections.isEmpty == false) {
      val closest = intersections.reduce((p1, p2) => Origin.closestManhattanDistance(p1, p2)._1)
      Some(Origin.manhattanDistance(closest))
    } else {
      None
    }
  }

  /** Returns a sequence of `WiringInstruction` elements representing the provided path.
   *
   * @throws NullPointerException if `path` is `null`.
   */
  def getWiringInstructions(path: NonBlankString): Vector[WiringInstruction] = {
    if (path == null) throw new NullPointerException("`path` must not be `null`.")

    path.split(',').map(WiringInstruction(_)).toVector
  }

  /** Returns a sequence coordinates for wiring instructions.
   *
   * @throws IllegalArgumentException if `wi` is either `null` or empty.
   */
  def getCoordinates(wi: Vector[WiringInstruction]): Vector[Point] = {
    wi.requireNonEmpty("`wi` must not be empty.")

    val coordinates: Vector[Point] = wi.foldLeft(Vector[Point](Origin))((coordinates, instruction) => {
      coordinates.appendedAll(getCoordinatesForInstruction(coordinates.last, instruction))
    })

    coordinates
  }

  /** Returns a `Set` of points touched by a path specified by wiring instructions.
   *
   * @throws NullPointerException if `offset` is `null`.
   * @throws NullPointerException if `wiringInstruction` is `null`.
   */
  def getCoordinatesForInstruction(offset: Point, wi: WiringInstruction): Vector[Point] = {
    if (offset == null) throw new NullPointerException("`offset` must not be `null`.")
    if (wi == null) throw new NullPointerException("`wi` must not be `null`.")

    val points = wi.direction match {
      case LEFT =>
        val start = offset.x - 1
        val end = offset.x - wi.distance
        for (x <- start to end by -1) yield Point(x, offset.y)
      case RIGHT =>
        val start = offset.x + 1
        val end = offset.x + wi.distance
        for (x <- start to end) yield Point(x, offset.y)
      case UP =>
        val start = offset.y + 1
        val end = offset.y + wi.distance
        for (y <- start to end) yield Point(offset.x, y)
      case DOWN =>
        val start = offset.y - 1
        val end = offset.y - wi.distance
        for (y <- start to end by -1) yield Point(offset.x, y)
    }

    points.toVector
  }
}

case class Day03Result(part1: Int, part2: Int)