package net.halenka.hannes.aoc19scala

import net.halenka.hannes.aoc19scala.day03._
import net.halenka.hannes.aoc19scala.validation.{NonBlankString, SeqValidator, StringValidator}

import scala.util.{Failure, Success}

object Day03 {
  def answer: Either[Any, Int] = {
    loadTextFileResource("day03/input.txt") match {
      case Success(lines) =>
        calculateShortestDistance(lines.head, lines(1)) match {
          case Some(distance) => Right(distance)
          case None => Left("The two paths appear not to intersect each other.")
        }
      case Failure(ex) => Left(ex)
    }
  }

  /** Returns the <i>Manhattan Distance</i> between origin and the closes intersection between to paths.
   *
   * @throws IllegalArgumentException if either `path1` or `path2` is `null`.
   */
  def calculateShortestDistance(path1: String, path2: String): Option[Int] = {
    val _path1 = path1.requireNonBlank("`path1` must not be blank.")
    val _path2 = path2.requireNonBlank("`path2` must not be blank.")

    val wiringInstructions = Vector(getWiringInstructions(_path1), getWiringInstructions(_path2))
    val coordinates = wiringInstructions.map(getCoordinates)
    val intersections = coordinates.reduceLeft(_.intersect(_)).filter(_ != Point(0, 0))

    if (intersections.isEmpty == false) {
      val origin = Point(0, 0)
      val closest = intersections.reduce((p1, p2) => origin.closestManhattanDistance(p1, p2)._1)
      Some(origin.manhattanDistance(closest))
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

    val coordinates: Vector[Point] = wi.foldLeft(Vector[Point](Point(0, 0)))((coordinates, instruction) => {
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

