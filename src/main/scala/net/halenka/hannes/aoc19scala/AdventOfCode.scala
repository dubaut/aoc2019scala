package net.halenka.hannes.aoc19scala

object AdventOfCode extends App {
  println("*** Advent of Code 2019, done in Scala ***")

  println(s"Day 01: ${Day01.answer}")

  print("Day 02: ")
  Day02.answer match {
    case Right(answer) => println(answer)
    case Left(left) => println(s"ERROR: ${left.toString}")
  }

  print("Day 03: ")
  Day03.answer match {
    case Right(answer) => println(answer)
    case Left(left) => println(s"ERROR: ${left.toString}")
  }
}