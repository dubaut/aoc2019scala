package net.halenka.hannes.aoc19scala.intcode

sealed trait Instruction {
  val length: Int
}

object Instruction {
}

abstract class AddOrMultiply(val readAddr1: Int, val readAddr2: Int, val storeAddr: Int) extends Instruction {
  require(readAddr1 >= 0)
  require(readAddr2 >= 0)
  require(storeAddr >= 0)

  override val length: Int = 4
}

sealed trait InstructionWithInput extends Instruction

/** Opcode 1 */
case class Add(override val readAddr1: Int, override val readAddr2: Int, override val storeAddr: Int) extends AddOrMultiply(readAddr1, readAddr2, storeAddr)

/** Opcode 2 */
case class Multiply(override val readAddr1: Int, override val readAddr2: Int, override val storeAddr: Int) extends AddOrMultiply(readAddr1, readAddr2, storeAddr)

/** Opcode 99 */
case class Terminate() extends Instruction {
  override val length: Int = 1
}

/** Opcode 3 */
case class StoreInput(storeAddr: Int) extends InstructionWithInput {
  override val length: Int = 2
}