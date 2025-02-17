package wacc.back_end

sealed trait Instr

case class ADD(op1: Reg | MemAddr, op2: Reg | Imm | MemAddr) extends Instr
case class SUB(op1: Reg | MemAddr, op2: Reg | Imm | MemAddr) extends Instr
case class DIV(op1: Reg | MemAddr, op2: Reg | Imm | MemAddr) extends Instr
case class MUL(op1: Reg | MemAddr, op2: Reg | Imm | MemAddr) extends Instr

case class PUSH(op: Reg | MemAddr | Imm) extends Instr
case class POP() extends Instr


case class MOV(op1: Reg | MemAddr, op2: Reg | Imm | MemAddr) extends Instr
case class RET() extends Instr
case class CALL(op: Label) extends Instr

// Can be used to align rsp to 16 byte boundary
// ie `and rsp, -16`
// case class AND(op1: Reg | MemAddr | Imm, ) extends Instr

// //TODO
// case class CALL(op1: Reg | MemAddr | Imm, ) extends Instr
// case class LEA(op1: Reg | MemAddr | Imm, ) extends Instr
// case class JMP(op1: Reg | MemAddr | Imm, ) extends Instr
// case class AND(op1: Reg | MemAddr | Imm, ) extends Instr
// case class CMP(op1: Reg | MemAddr | Imm, ) extends Instr
// case class JE(op1: Reg | MemAddr | Imm, ) extends Instr
// case class SETE(op1: Reg | MemAddr | Imm, ) extends Instr
// case class JNE(op1: Reg | MemAddr | Imm, ) extends Instr
// case class SETL(op1: Reg | MemAddr | Imm, ) extends Instr
