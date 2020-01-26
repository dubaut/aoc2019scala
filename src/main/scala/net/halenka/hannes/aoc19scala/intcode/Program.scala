package net.halenka.hannes.aoc19scala.intcode

import net.halenka.hannes.aoc19scala.validation.SeqValidator

/**
 * @throws IllegalArgumentException if `steps` is either `null` or empty.
 */
case class Program(steps: IndexedSeq[Int]) {
  steps.requireNonEmpty("`steps` must not be `null` or empty.")

  /** Creates copy of this program with one value replaced.
   *
   * @throws IllegalArgumentException if `address` is less then '0' or greater than the highest address in the program
   */
  def updated(address: Int, value: Int): Program = {
    require(address >= 0, "`address` must not be less than '0'.")
    require(address < steps.size)

    Program(steps.updated(address, value))
  }

  /** The number of steps in the program. */
  def size: Int = steps.size
}

object Program {
  def apply(steps: Int*): Program = Program(steps.toIndexedSeq)
}
