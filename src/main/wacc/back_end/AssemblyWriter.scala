package wacc.back_end

import instrument._

object AssemblyWriter {
  val finalAssembly = StringBuilder()

  def generateAsmFile(ir: (List[Section], List[LabelDef])) = {


    ir._2.map(labelHandler(_))
    
  }

  def labelHandler(labelDef: LabelDef): Unit = {
    labelDef match 
      case FuncLabelDef(name, instructions, localLabelDefs) => {
        finalAssembly.append(s"$name:")
        instructions.result.map(instructionHandler(_))
        localLabelDefs.result.map(labelHandler(_))
      }
      case wacc.back_end.LocalLabelDef(_, _) => ??? 
  }


  def instructionHandler(instr: Instr) = {
    instr match
      case ADD(ops) =>  ???
      case SUB(ops) => ???
      case IDIV(op) => ???
      case IMUL(ops) => ???
      case AND(ops) => ???
      case OR(ops) => ???
      case XOR(ops) => ???
      case NEG(op) => ???
      case NOT(op) => ???
      case MOV(op) => ???
      case PUSH(op) => ???
      case POP(op) => ???
      case CMP(ops) => ???
      case SETCond(cond, op) => ???
      case RET() => ???
      case CALL(op) => ???
      case CDQ => ???
  }
}