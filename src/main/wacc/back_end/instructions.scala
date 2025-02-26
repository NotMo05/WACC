package wacc.back_end

sealed trait Instr

enum Cond {
  case E, NE, L, LE, G, GE, A, AE, B, BE, S, NS, O, NO, P, NP

  override def toString(): String = s"J${this.productPrefix.toLowerCase}"
}

// If someone can think of some better names, please change these
type LocationOps = (Reg, Location) | (Location, Reg)
type FullOps = LocationOps | (Location, Imm) //This disallows mem to mem operations


case class ADD(ops: FullOps) extends Instr
case class SUB(ops: FullOps) extends Instr
case class IDIV(op: Location) extends Instr
case class IMUL(ops: LocationOps) extends Instr


case class AND(ops: FullOps) extends Instr
case class OR(ops: FullOps) extends Instr
case class XOR(ops: FullOps) extends Instr

case class NEG(op: Location) extends Instr
case class NOT(op: Location) extends Instr

case class MOV(ops: FullOps | (Location, Reg | Imm)) extends Instr
case class PUSH(op: Operand) extends Instr
case class POP(op: Location) extends Instr

case class CMP(ops: FullOps) extends Instr
case class SETCond(cond: Cond, op: Location) extends Instr
case class JCond(cond: Cond, label: Label) extends Instr

case class RET() extends Instr
case class CALL(op: Label) extends Instr

case object CDQ extends Instr

// TODO
// case class CALL() extends Instr
// case class LEA() extends Instr
// case class JMP() extends Instr


