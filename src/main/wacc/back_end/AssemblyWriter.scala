package wacc.back_end

import Cond._
import java.io.PrintWriter
import java.nio.file.Paths

object AssemblyWriter {
  val assemblyBuilder = List.newBuilder[String]

  def generateAsmFile(ir: (List[Section], List[LabelDef]), filename: String) = {
    val file = Paths.get(filename).getFileName.toString
    ir._2.map(labelHandler(_))
    val finalAssembly = assemblyBuilder.result()
    
    val writer = new PrintWriter(s"${file.dropRight(5)}.s")
    val boilerplate = List(
      ".intel_syntax noprefix",
      ".globl main",
      ".section .rodata",
      ".text")
    boilerplate.foreach(writer.println)
    finalAssembly.foreach(writer.println)
    writer.close()
    
  }

  def labelHandler(labelDef: LabelDef): Unit = {
    labelDef match 
      case FuncLabelDef(name, instructions, localLabelDefs) => {
        assemblyBuilder += (s"$name:")
        instructions.result.map(instructionHandler(_))
        localLabelDefs.result.map(labelHandler(_))
      }
      case wacc.back_end.LocalLabelDef(name, instructions) => {
        assemblyBuilder += (s"$name:")
        instructions.result.map(instructionHandler(_))
      }
  }


  def instructionHandler(instr: Instr) = {
    assemblyBuilder += (
      instr match
        case ADD((op1, op2)) => s"add $op1, $op2 "
        case SUB((op1, op2)) => s"sub $op1, $op2 "
        case IDIV(op) => s"div $op"
        case IMUL((op1, op2)) => s"mul $op1, $op2 "
        case AND((op1, op2)) => s"and $op1, $op2 "
        case OR((op1, op2)) => s"or $op1, $op2"
        case XOR((op1, op2)) => s"xor $op1, $op2"
        case NEG(op) => s"neg $op"
        case NOT(op) => s"not $op"
        case MOV((op1, op2)) => s"mov $op1, $op2 "
        case PUSH(op) => s"push $op"
        case POP(op) => s"pop $op"
        case CMP((op1, op2)) => s"cmp $op1, $op2 "
        case SETCond(cond, op) => "SET" + condToAsm(cond) + s"$op"
        case RET() => s"ret"
        case CALL(op) => s"call $op"
        case CDQ => "cdq"
        case JCond(cond, label) => "j" + condToAsm(cond) + s"$label"

    )
  }

  def condToAsm(cond: Cond) = {
    cond match
      case E => "e"
      case NE => "ne"
      case L => "l"
      case LE => "le"
      case G => "g"
      case GE => "ge"
      case A => "a"
      case AE => "ae"
      case B => "b"
      case BE => "be"
      case S => "s"
      case NS => "n"
      case O => "o"
      case NO => "no"
      case P => "p"
      case NP => "np"
  }
}
