package net.halenka.hannes.aoc19scala.day04

object Day04 {
  final val PasswordRange = 234208 to 765869
  final val RegexEqualAdjacentDigits = "^.*(.)\\1+.*$".r

  def answer: Either[Any, Day04Result] = {
    val part1 = calculatePart1(PasswordRange)

    val result = Day04Result(part1.size)

    Right(result)
  }

  def calculatePart1(range: Range): IndexedSeq[Int] = {
    require(range != null, "`range` must not be `null`.")

    range.filter(hasEqualAdjacentDigits).filter(digitsNeverDecrease)
  }

  def hasEqualAdjacentDigits(value: Int): Boolean = RegexEqualAdjacentDigits.matches(String.valueOf(value))

  def digitsNeverDecrease(value: Int): Boolean = {
    val intSeq = String.valueOf(value).map(_.toInt)
    val sortedIntSeq = intSeq.sorted

    intSeq == sortedIntSeq
  }
}

case class Day04Result(part1: Int)