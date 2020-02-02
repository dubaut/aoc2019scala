package net.halenka.hannes.aoc19scala.intcode

sealed trait Instruction {
  val length: Int
}

object Instruction {

  abstract class AddOrMultiply(val readAddr1: Parameter, val readAddr2: Parameter, val storeAddr: Parameter) extends Instruction {
    require(readAddr1 != null)
    require(readAddr2 != null)
    require(storeAddr != null)

    override val length: Int = 4
  }

  sealed trait InstructionWithInput extends Instruction

  /** Opcode 1 */
  final case class Add(override val readAddr1: Parameter, override val readAddr2: Parameter, override val storeAddr: Parameter) extends AddOrMultiply(readAddr1, readAddr2, storeAddr)

  /** Opcode 2 */
  final case class Multiply(override val readAddr1: Parameter, override val readAddr2: Parameter, override val storeAddr: Parameter) extends AddOrMultiply(readAddr1, readAddr2, storeAddr)

  /** Opcode 3 */
  final case class StoreInput(storeAddr: Parameter) extends InstructionWithInput {
    override val length: Int = 2
  }

  /** Opcode 4 */
  final case class Output(addr: Parameter) extends Instruction {
    override val length: Int = 2
  }

  /** Opcode 99 */
  final case class Terminate() extends Instruction {
    override val length: Int = 1
  }

}