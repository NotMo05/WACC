package wacc.back_end


sealed trait Instr

enum Cond {
  case E, NE, L, LE, G, GE, A, AE, B, BE, S, NS, O, NO, P, NP
}

case class ADD(op1: Location, op2: Operand) extends Instr
case class SUB(op1: Location, op2: Operand) extends Instr
case class DIV(op1: Location, op2: Operand) extends Instr
case class MUL(op1: Location, op2: Operand) extends Instr

case class AND(op1: Location, op2: Operand) extends Instr
case class OR(op1: Location, op2: Operand) extends Instr
case class XOR(op1: Location, op2: Operand) extends Instr
case class NOT(op: Location) extends Instr

case class PUSH(op: Operand) extends Instr
case class POP(op: Location) extends Instr
case class MOV(op1: Location, op2: Operand) extends Instr


case class CMP(op1: Location, op2: Operand) extends Instr
case class SETCond(cond: Cond, op: Location) extends Instr
case class RET() extends Instr
case class CALL(op: Label) extends Instr

// Can be used to align rsp to 16 byte boundary
// ie `and rsp, -16`
// case class AND(op1: Operand, ) extends Instr

// //TODO
// case class CALL(op1: Operand, ) extends Instr
// case class LEA(op1: Operand, ) extends Instr
// case class JMP(op1: Operand, ) extends Instr

// case class JE(op1: Operand, ) extends Instr
// case class JNE(op1: Operand, ) extends Instr

