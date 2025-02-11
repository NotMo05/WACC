package wacc

sealed trait Instr


case class ADD(op1: Reg | MemAddr, op2: Reg | Imm | MemAddr) extends Instr
case class SUB(op1: Reg | MemAddr, op2: Reg | Imm | MemAddr) extends Instr
case class DIV(op1: Reg | MemAddr, op2: Reg | Imm | MemAddr) extends Instr
case class MUL(op1: Reg | MemAddr, op2: Reg | Imm | MemAddr) extends Instr