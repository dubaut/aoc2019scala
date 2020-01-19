package net.halenka.hannes.aoc19scala.day04

import net.halenka.hannes.aoc19scala.IntOps

object Day04 {
  final val PasswordRange = 234208 to 765869
  final val RegexEqualAdjacentDigits = "(?:(.)\\1+)".r

  def answer: Either[Any, Day04Result] = {
    val part1 = calculatePart1(PasswordRange)
    val part2 = calculatePart2(part1)

    val result = Day04Result(part1.size, part2)

    Right(result)
  }

  def calculatePart1(range: Range): IndexedSeq[Int] = {
    require(range != null, "`range` must not be `null`.")

    range.filter(hasEqualAdjacentDigits).filter(digitsNeverDecrease)
  }

  def calculatePart2(part1: IndexedSeq[Int]): Int = part1.count(hasTwoEqualAdjacentDigits)

  def hasEqualAdjacentDigits(value: Int): Boolean = {
    RegexEqualAdjacentDigits.findFirstIn(String.valueOf(value)) match {
      case Some(_) => true
      case None => false
    }
  }

  def hasTwoEqualAdjacentDigits(value: Int): Boolean = {
    RegexEqualAdjacentDigits.findAllIn(String.valueOf(value)).find(_.size == 2) match {
      case Some(_) => true
      case None => false
    }
  }

  def digitsNeverDecrease(value: Int): Boolean = {
    value.toIndexedSeq == value.toIndexedSeq.sorted
  }
}

case class Day04Result(part1: Int, part2: Int)