package net.halenka.hannes.aoc19scala

import net.halenka.hannes.aoc19scala.day01.Day01
import net.halenka.hannes.aoc19scala.day02.Day02
import net.halenka.hannes.aoc19scala.day03.Day03

object AdventOfCode extends App {
  println("*** Advent of Code 2019, done in Scala ***")

  print("Day 01: ")
  Day01.answer match {
    case Right(answer) => println(answer)
    case Left(left) => println(s"ERROR: ${left.toString}")
  }

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