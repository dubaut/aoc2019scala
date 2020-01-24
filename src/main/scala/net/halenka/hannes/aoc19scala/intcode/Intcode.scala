package net.halenka.hannes.aoc19scala.intcode

import net.halenka.hannes.aoc19scala.validation.{NonEmptySeq, SeqValidator}
import net.halenka.hannes.aoc19scala.{Result, RuntimeError}

class Intcode private(private val program: NonEmptySeq[Int]) {
  assert(program != null)

  /** Runs the program and returns the memory. */
  def run(noun: Int, verb: Int): Seq[Int] = {
    val memory: Seq[Int] = program.updated(1, noun).updated(2, verb)

    Intcode.run(memory.toIndexedSeq) match {
      case (program, _) => program
    }
  }
}

object Intcode {
  type Program = IndexedSeq[Int]

  /** Creates a new `Intcode` instance.
   *
   * @param program The program to be executed.
   * @throws IllegalArgumentException when `program` is either <null> or an empty `Seq`.
   */
  def apply(program: Program): Intcode = new Intcode(program.requireNonEmpty("`program` must not be empty."))

  /** Processes all instructions of a program.
   *
   * @throws IllegalArgumentException if `program` is `null` or empty
   * @throws IllegalArgumentException if `address` is less then '0'
   * @return a tuple containing the memory (`_1`) and the output (`_2`) after running the program.
   */
  @scala.annotation.tailrec
  def run(program: Program, address: Int = 0): (IndexedSeq[Int], IndexedSeq[Int]) = {
    program.requireNonEmpty("`program` must not be `null` or empty.")
    require(address >= 0, "`address` must not be less than '0'.")

    loadInstruction(program, address) match {
      case Right(instruction) =>
        instruction match {
          case Terminate() => (program, IndexedSeq[Int]())
          case instruction => applyInstruction(instruction, program) match {
            case (program, output) => run(program, address + instruction.length)
          }
        }
      case Left(error) => throw new RuntimeException(s"Error: $error")
    }
  }

  /** Loads the instruction from the program at the specified address.
   *
   * @throws IllegalArgumentException if `program` is either `null` or empty
   * @throws IllegalArgumentException if `address` is either < 0 or >= program.size
   */
  def loadInstruction(program: Program, address: Int): Result[Instruction] = {
    program.requireNonEmpty("`program` must not be empty.")
    require(address >= 0, "`address` must not be less than '0.")
    require(address < program.size, s"'$address' is not a valid address for the specified program.")

    val instruction = program.drop(address)

    instruction.head match {
      case 1 =>
        if (instruction.length >= 4) {
          Right(Add(instruction(1), instruction(2), instruction(3)))
        } else {
          Left(UnexpectedEndOfInstructionError())
        }
      case 2 =>
        if (instruction.length >= 4) {
          Right(Multiply(instruction(1), instruction(2), instruction(3)))
        } else {
          Left(UnexpectedEndOfInstructionError())
        }
      case 3 =>
        if (instruction.length >= 2) {
          Right(StoreInput(instruction(1)))
        } else {
          Left(UnexpectedEndOfInstructionError())
        }
      case 99 =>
        Right(Terminate())
      case opcode => Left(InvalidOpcodeError(opcode))
    }
  }

  type ApplyInstructionResult = (Program, Option[Int])

  /** Applies an instruction to a program.
   *
   * @throws IllegalArgumentException if `instruction` is `null`
   * @throws IllegalArgumentException if `program` is either `null` or empty
   * @return the modified program and the optional output
   */
  def applyInstruction(instruction: Instruction, program: Program): ApplyInstructionResult = {
    require(instruction != null, "`instruction` must not be `null`.")
    program.requireNonEmpty("`program` must not be `null` or empty.")

    def addOrMultiply(instruction: AddOrMultiply, program: IndexedSeq[Int], f: (Int, Int) => Int): IndexedSeq[Int] = {
      require(instruction != null)
      require(program != null && program.nonEmpty)
      require(f != null)

      val result = f(program(instruction.readAddr1), program(instruction.readAddr2))

      program.updated(instruction.storeAddr, result)
    }

    instruction match {
      case instruction: Add =>
        val result = addOrMultiply(instruction, program, (a: Int, b: Int) => a + b)
        (result, None)
      case instruction: Multiply =>
        val result = addOrMultiply(instruction, program, (a: Int, b: Int) => a * b)
        (result, None)
      case _: Terminate => (program, None)
      case _ => throw new UnsupportedInstructionException(instruction)
    }
  }

  /** Applies a instruction to a program, using the provided input.
   *
   * @return the modified program and the optional output
   * @throws IllegalArgumentException if `instruction` is `null`
   * @throws IllegalArgumentException if `program` is either `null` or empty
   */
  def applyInstructionWithInput(instruction: InstructionWithInput,
                                program: Program,
                                input: Int): ApplyInstructionResult = {
    require(instruction != null, "`instruction` must not be ´null´.")
    program.requireNonEmpty("´program´ must not be `null` or empty.")

    instruction match {
      case StoreInput(storeAddr) =>
        val result = program.updated(storeAddr, input)
        (result, None)
      case _ => throw new UnsupportedInstructionException(instruction)
    }
  }
}

/** Indicates that a opcode is unknown or unsupported. */
case class InvalidOpcodeError(opcode: Int) extends RuntimeError(s"Invalid opcode: '$opcode'")

@deprecated
class InvalidOpcodeException(opcode: Int) extends RuntimeException(s"Invalid opcode: '${opcode}'")

/** Indicates that an instruction does not have a sufficient number of parameter. */
case class UnexpectedEndOfInstructionError() extends RuntimeError

@deprecated
class UnexpectedEndOfInstructionException extends RuntimeException

class UnsupportedInstructionException(instruction: Instruction) extends RuntimeException(s"The instruction type '${instruction}' is not supported.")