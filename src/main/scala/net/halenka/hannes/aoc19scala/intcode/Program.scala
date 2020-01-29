package net.halenka.hannes.aoc19scala.intcode

import net.halenka.hannes.aoc19scala.Result
import net.halenka.hannes.aoc19scala.intcode.Instruction._
import net.halenka.hannes.aoc19scala.intcode.ParameterMode.Position
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

  /** Returns the instruction present at the specified address of a program.
   *
   * @throws IllegalArgumentException if the address is less than '0', or greater than the last address of the program
   */
  def getInstruction(address: Int): Result[Instruction] = {
    require(address >= 0, "The address must not be less than '0'.")
    require(address < size, s"The address must not be greater than the last address ('${size - 1}') in the program.")

    val instruction = steps.drop(address)
    val opcode = instruction.head

    opcode match {
      case 1 =>
        if (instruction.size >= 4) {
          Right(Add(Parameter(instruction(1), Position), Parameter(instruction(2), Position), Parameter(instruction(3), Position)))
        } else {
          Left(UnexpectedEndOfInstructionError())
        }
      case 2 =>
        if (instruction.size >= 4) {
          Right(Multiply(Parameter(instruction(1), Position), Parameter(instruction(2), Position), Parameter(instruction(3), Position)))
        } else {
          Left(UnexpectedEndOfInstructionError())
        }
      case 3 =>
        if (instruction.size >= 2) {
          Right(StoreInput(Parameter(instruction(1), Position)))
        } else {
          Left(UnexpectedEndOfInstructionError())
        }
      case 99 => Right(Terminate())
      case opcode => Left(InvalidOpcodeError(opcode))
    }
  }

  type ApplyInstructionResult = (Program, Option[Int])

  /** Applies a instruction to the program.
   *
   * @return a copy of the program in the state after applying the instruction and optional output. If an instruction
   *         does not modify a program the same instance is returned.
   * @throws IllegalArgumentException        if the function is null.
   * @throws UnsupportedInstructionException if the instruction is an InstructionWithInput
   */
  def applyInstruction(instruction: Instruction): ApplyInstructionResult = {
    require(instruction != null, "The instruction must not be null.")

    def addOrMultiply(instruction: AddOrMultiply, f: (Int, Int) => Int): Program = {
      require(instruction != null)
      require(f != null)

      val result = f(steps(instruction.readAddr1.value), steps(instruction.readAddr2.value))

      updated(instruction.storeAddr.value, result)
    }

    instruction match {
      case add: Add =>
        val result = addOrMultiply(add, (a: Int, b: Int) => a + b)
        (result, None)
      case multiply: Multiply =>
        val result = addOrMultiply(multiply, (a: Int, b: Int) => a * b)
        (result, None)
      case _: Terminate => (this, None)
      case _ => throw new UnsupportedInstructionException(instruction)
    }
  }
}

object Program {
  def apply(steps: Int*): Program = Program(steps.toIndexedSeq)
}