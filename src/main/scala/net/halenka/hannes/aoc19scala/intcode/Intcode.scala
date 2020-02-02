package net.halenka.hannes.aoc19scala.intcode

import net.halenka.hannes.aoc19scala.RuntimeError
import net.halenka.hannes.aoc19scala.intcode.Instruction._

class Intcode private(private val program: Program) {
  assert(program != null)

  /** Runs the program and returns the memory. */
  def run(noun: Int, verb: Int): Seq[Int] = {
    val memory = program.updated(1, noun).updated(2, verb)

    Intcode.run(memory) match {
      case (program, _) => program
    }
  }
}

object Intcode {
  /** Creates a new `Intcode` instance.
   *
   * @param program The program to be executed.
   * @throws IllegalArgumentException if `program` is `null`
   */
  def apply(program: Program): Intcode = {
    require(program != null, "`program` must not be `null`.")
    new Intcode(program)
  }

  /** Processes all instructions of a program.
   *
   * @throws IllegalArgumentException if `program` is `null`
   * @throws IllegalArgumentException if `address` is less then '0'
   * @return a tuple containing the memory (`_1`) and the output (`_2`) after running the program.
   */
  @scala.annotation.tailrec
  def run(program: Program, address: Int = 0): (IndexedSeq[Int], IndexedSeq[Int]) = {
    require(program != null, "`program` must not be `null`.")
    require(address >= 0, "`address` must not be less than '0'.")

    program.getInstruction(address) match {
      case Right(instruction) =>
        instruction match {
          case Terminate() => (program.steps, IndexedSeq[Int]())
          case instruction: InstructionWithInput => program.applyInstructionWithInput(instruction, 1) match {
            case (program, output) => run(program, address + instruction.length)
          }
          case instruction => program.applyInstruction(instruction) match {
            case (program, output) => run(program, address + instruction.length)
          }
        }
      case Left(error) => throw new RuntimeException(s"Error: $error")
    }
  }
}