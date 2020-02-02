package net.halenka.hannes.aoc19scala.intcode

import net.halenka.hannes.aoc19scala.intcode.Instruction._
import net.halenka.hannes.aoc19scala.intcode.Parameter.ParameterMode
import net.halenka.hannes.aoc19scala.intcode.Parameter.ParameterMode.{Immediate, Position}
import net.halenka.hannes.aoc19scala.intcode.Program.{InvalidAddressException, InvalidOpcodeError, UnexpectedEndOfInstructionError, UnsupportedInstructionException}
import net.halenka.hannes.aoc19scala.validation.SeqValidator
import net.halenka.hannes.aoc19scala.{Result, RuntimeError}
import org.apache.commons.lang3.StringUtils

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
    require(address < size, s"The address must not be greater than the greatest address ('${size - 1}') in the program.")

    def getParameterModes(code: String, size: Int): IndexedSeq[ParameterMode] = {
      require(code != null)
      require(size >= 1)

      StringUtils.leftPad(code, size, "0").reverseIterator.map {
        case '0' => Position
        case '1' => Immediate
        case mode => throw new RuntimeException(s"Illegal mode: '$mode'.")
      }.toIndexedSeq
    }

    val instruction = steps.drop(address)
    val instructionHead = instruction.head.toString

    val opcode = instructionHead.takeRight(2).toInt

    opcode match {
      case 1 =>
        if (instruction.size >= 4) {
          val parameterModes = getParameterModes(instructionHead.dropRight(2), 3)
          Right(Add(Parameter(instruction(1), parameterModes(0)), Parameter(instruction(2), parameterModes(1)), Parameter(instruction(3), parameterModes(2))))
        } else {
          Left(UnexpectedEndOfInstructionError())
        }
      case 2 =>
        if (instruction.size >= 4) {
          val parameterModes = getParameterModes(instructionHead.dropRight(2), 3)
          Right(Multiply(Parameter(instruction(1), parameterModes(0)), Parameter(instruction(2), parameterModes(1)), Parameter(instruction(3), parameterModes(2))))
        } else {
          Left(UnexpectedEndOfInstructionError())
        }
      case 3 =>
        if (instruction.size >= 2) {
          val parameterModes = getParameterModes(instructionHead.dropRight(2), 1)
          Right(StoreInput(Parameter(instruction(1), parameterModes(0))))
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

      val result = f(getValue(instruction.readAddr1), getValue(instruction.readAddr2))

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

  /** Returns the value for a parameter.
   *
   * @throws IllegalArgumentException if the parameter is null
   */
  def getValue(parameter: Parameter): Int = {
    require(parameter != null, "The parameter must not be null.")

    parameter.mode match {
      case ParameterMode.Position =>
        if (isValidAddress(parameter.value)) steps(parameter.value)
        else throw new InvalidAddressException(parameter.value)
      case ParameterMode.Immediate => parameter.value
    }
  }

  /** Returns `true` if the address is valid for the program. */
  def isValidAddress(address: Int): Boolean = address > -1 && address < size

  /** Applies a instruction to the program, using the provided input.
   *
   * @return the modified program and the optional output
   * @throws IllegalArgumentException if `instruction` is `null`
   */
  def applyInstructionWithInput(instruction: InstructionWithInput, input: Int): ApplyInstructionResult = {
    require(instruction != null, "`instruction` must not be ´null´.")

    instruction match {
      case StoreInput(storeAddr) =>
        val result = updated(storeAddr.value, input)
        (result, None)
      case _ => throw new UnsupportedInstructionException(instruction)
    }
  }
}

object Program {
  def apply(steps: Int*): Program = Program(steps.toIndexedSeq)

  /** Indicates that a opcode is unknown or unsupported. */
  final case class InvalidOpcodeError(opcode: Int) extends RuntimeError(s"Invalid opcode: '$opcode'")

  /** Indicates that an instruction does not have a sufficient number of parameter. */
  final case class UnexpectedEndOfInstructionError() extends RuntimeError

  class UnsupportedInstructionException(instruction: Instruction) extends RuntimeException(s"The instruction type '$instruction' is not supported.")

  /** Indicates that a accessed address does not exist. */
  class InvalidAddressException(address: Int) extends RuntimeException(s"The address '$address' does not exist.")

}