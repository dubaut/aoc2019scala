package net.halenka.hannes.aoc19scala.day03

case class Point(x: Int, y: Int) {
  /** Returns the Manhattan Distance between this and another `Point`.
   *
   * @param that The point to which the distance is calculated.
   * @throws NullPointerException if `that` is `null`.
   */
  def manhattanDistance(that: Point): Int = {
    require(that != null, "`that` must not be `null`.")

    (this.x - that.x).abs + (this.y - that.y).abs
  }

  /** Returns the point with the closest Manhattan Distance and the calculated distance.
   *
   * @throws IllegalArgumentException if either `point1` or `point2` is `null`.
   */
  def closestManhattanDistance(point1: Point, point2: Point): (Point, Int) = {
    require(point1 != null, "`point1` must not be `null`.")
    require(point2 != null, "`point2` must not be `null`.")

    val distance1 = manhattanDistance(point1)
    val distance2 = manhattanDistance(point2)

    if (distance1 <= distance2) {
      (point1, distance1)
    } else {
      (point2, distance2)
    }
  }
}