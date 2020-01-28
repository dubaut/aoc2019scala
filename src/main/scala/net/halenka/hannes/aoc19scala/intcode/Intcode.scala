package net.halenka.hannes.aoc19scala.intcode

import net.halenka.hannes.aoc19scala.intcode.Instruction._
import net.halenka.hannes.aoc19scala.{Result, RuntimeError}

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
          case instruction: InstructionWithInput => applyInstructionWithInput(instruction, program, 1) match {
            case (program, output) => run(program, address + instruction.length)
          }
          case instruction => applyInstruction(instruction, program) match {
            case (program, output) => run(program, address + instruction.length)
          }
        }
      case Left(error) => throw new RuntimeException(s"Error: $error")
    }
  }

  type ApplyInstructionResult = (Program, Option[Int])

  /** Applies an instruction to a program.
   *
   * @throws IllegalArgumentException if `instruction` is `null`
   * @throws IllegalArgumentException if `program` is either `null`
   * @return the modified program and the optional output
   */
  def applyInstruction(instruction: Instruction, program: Program): ApplyInstructionResult = {
    require(instruction != null, "`instruction` must not be `null`.")
    require(program != null, "`program` must not be `null` or empty.")

    def addOrMultiply(instruction: AddOrMultiply, program: IndexedSeq[Int], f: (Int, Int) => Int): IndexedSeq[Int] = {
      require(instruction != null)
      require(program != null && program.nonEmpty)
      require(f != null)

      val result = f(program(instruction.readAddr1.value), program(instruction.readAddr2.value))

      program.updated(instruction.storeAddr.value, result)
    }

    instruction match {
      case instruction: Add =>
        val result = addOrMultiply(instruction, program.steps, (a: Int, b: Int) => a + b)
        (Program(result), None)
      case instruction: Multiply =>
        val result = addOrMultiply(instruction, program.steps, (a: Int, b: Int) => a * b)
        (Program(result), None)
      case _: Terminate => (program, None)
      case _ => throw new UnsupportedInstructionException(instruction)
    }
  }

  /** Applies a instruction to a program, using the provided input.
   *
   * @return the modified program and the optional output
   * @throws IllegalArgumentException if `instruction` is `null`
   * @throws IllegalArgumentException if `program` is either `null`
   */
  def applyInstructionWithInput(instruction: InstructionWithInput,
                                program: Program,
                                input: Int): ApplyInstructionResult = {
    require(instruction != null, "`instruction` must not be ´null´.")
    require(program != null, "´program´ must not be `null` or empty.")

    instruction match {
      case StoreInput(storeAddr) =>
        val result = program.updated(storeAddr.value, input)
        (result, None)
      case _ => throw new UnsupportedInstructionException(instruction)
    }
  }
}

/** Indicates that a opcode is unknown or unsupported. */
final case class InvalidOpcodeError(opcode: Int) extends RuntimeError(s"Invalid opcode: '$opcode'")

@deprecated
class InvalidOpcodeException(opcode: Int) extends RuntimeException(s"Invalid opcode: '$opcode'")

/** Indicates that an instruction does not have a sufficient number of parameter. */
final case class UnexpectedEndOfInstructionError() extends RuntimeError

@deprecated
class UnexpectedEndOfInstructionException extends RuntimeException

class UnsupportedInstructionException(instruction: Instruction) extends RuntimeException(s"The instruction type '$instruction' is not supported.")