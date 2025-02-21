package wacc.back_end


sealed trait Instr

type LocationOps = (Reg, Location) | (Location, Reg)
type FullOps = LocationOps | (Location, Imm)
// If someone can think of some better names, please change these

enum Cond {
  case E, NE, L, LE, G, GE, A, AE, B, BE, S, NS, O, NO, P, NP
}

case class ADD(ops: FullOps) extends Instr
case class SUB(ops: FullOps) extends Instr
case class IDIV(op: Location) extends Instr
case class IMUL(ops: LocationOps) extends Instr


case class AND(ops: FullOps) extends Instr
case class OR(ops: FullOps) extends Instr
case class XOR(ops: FullOps) extends Instr

case class NEG(op: Location) extends Instr
case class NOT(op: Location) extends Instr

// Mov takes some other weird Operands, maybe unecessary?
case class MOV(op: (Location, Operand)) extends Instr
case class PUSH(op: Operand) extends Instr
case class POP(op: Location) extends Instr

case class CMP(ops: FullOps) extends Instr
case class SETCond(cond: Cond, op: Location) extends Instr

case class RET() extends Instr
case class CALL(op: Label) extends Instr

case object CDQ extends Instr

// Can be used to align rsp to 16 byte boundary
// ie `and rsp, -16`
// case class AND(op1: Operand, ) extends Instr

// //TODO
// case class CALL(op1: Operand, ) extends Instr
// case class LEA(op1: Operand, ) extends Instr
// case class JMP(op1: Operand, ) extends Instr

// case class JE(op1: Operand, ) extends Instr
// case class JNE(op1: Operand, ) extends Instr

