package wacc.back_end

import Cond._
import java.io.PrintWriter
import java.nio.file.Paths

object AssemblyWriter {
  val assemblyBuilder = List.newBuilder[String]

  def generateAsmFile(ir: (List[IR.Section], List[FuncLabelDef]), filename: String, folder: String = "") = {
    val file = Paths.get(filename).getFileName.toString
    ir._2.map(labelHandler(_))
    val finalAssembly = assemblyBuilder.result()
    assemblyBuilder.clear()
    
    val writer = new PrintWriter(s"$folder${file.dropRight(5)}.s")
    val boilerplate = List(
      ".intel_syntax noprefix",
      ".globl main",
      ".section .rodata",
      ".text")
    boilerplate.foreach(writer.println)
    finalAssembly.foreach(writer.println)
    writer.close()
    
  }

  def labelHandler(label: FuncLabelDef): Unit = {
        assemblyBuilder += (s"${label.name}:")
        label.instructions.result.map(instructionHandler(_))
  }

  def instructionHandler(instr: Instr) = {
    assemblyBuilder += (
      (instr: @unchecked) match
        case ADD((op1, op2)) => s"  add $op1, $op2"
        case SUB((op1, op2)) => s"  sub $op1, $op2"
        case IDIV(op) => s"  idiv $op"
        case IMUL((op1, op2)) => s"  imul $op1, $op2"
        case AND((op1, op2)) => s"  and $op1, $op2"
        case OR((op1, op2)) => s" or $op1, $op2"
        case XOR((op1, op2)) => s"  xor $op1, $op2"
        case NEG(op) => s"  neg $op"
        case NOT(op) => s"  not $op"
        case MOV((op1, op2)) => s"  mov $op1, $op2"
        case PUSH(op) => s"  push $op"
        case POP(op) => s"  pop $op"
        case CMP((op1, op2)) => s"  cmp $op1, $op2"
        case SETCond(cond, op) => s"  set${condToAsm(cond)} $op"
        case RET => "  ret"
        case CALL(op) => s" call $op"
        case CDQ => "  cdq"
        case JCond(cond, label) => s"  j${condToAsm(cond)} $label"
        case LEA((op1, op2)) => s"  lea $op1, $op2"
        case WhileIfLabel(num) => s".L$num:"
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
      case AL => "mp"
  }
}
