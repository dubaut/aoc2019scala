package net.halenka.hannes.aoc19scala.intcode

import net.halenka.hannes.aoc19scala.intcode.Intcode._
import net.halenka.hannes.aoc19scala.validation.{NonEmptySeq, SeqValidator}

class Intcode private(private val program: NonEmptySeq[Int]) {
  assert(program != null)

  /** Runs the program and returns the memory. */
  def run(noun: Int, verb: Int): Seq[Int] = {
    val memory: Seq[Int] = program.updated(1, noun).updated(2, verb)

    processInstruction(memory, 0)
  }
}

object Intcode {
  /** Creates a new `Intcode` instance.
   *
   * @param program The program to be executed.
   * @throws IllegalArgumentException when `program` is either <null> or an empty `Seq`.
   */
  def apply(program: Seq[Int]): Intcode = new Intcode(program.requireNonEmpty("`program` must not be empty."))

  /** Processes the instruction at the specified address and returns the modified memory.
   *
   * @throws IllegalArgumentException if `address` is not within the interval between `0` and `memory.size - 1`
   * @throws IllegalArgumentException if `memory` is either `null` or an empty `Seq`
   */
  @scala.annotation.tailrec
  def processInstruction(memory: Seq[Int], address: Int): Seq[Int] = {
    memory.requireNonEmpty("<memory> must not be empty.")
    if (address < 0 || address > memory.size - 1) throw new IllegalArgumentException(s"The address must not be less then `0` or greater than `${memory.size - 1}`")

    val instruction = memory.drop(address)
    val opcode = instruction.head

    opcode match {
      case 1 =>
        val processed = processOpcode(memory, (instruction(1), instruction(2)), instruction(3), (a: Int, b: Int) => a + b)
        processInstruction(processed, address + 4)
      case 2 =>
        val processed = processOpcode(memory, (instruction(1), instruction(2)), instruction(3), (a: Int, b: Int) => a * b)
        processInstruction(processed, address + 4)
      case 99 => memory
      case _ => throw new InvalidOpcodeException(opcode)
    }
  }

  private def processOpcode(memory: Seq[Int], read: (Int, Int), store: Int, f: (Int, Int) => Int): Seq[Int] = {
    val result = f(memory(read._1), memory(read._2))
    memory.updated(store, result)
  }
}

class InvalidOpcodeException(opcode: Int) extends RuntimeException(s"Invalid opcode: '${opcode}'")