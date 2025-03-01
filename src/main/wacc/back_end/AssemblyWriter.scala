package wacc.back_end

import Cond._
import java.io.PrintWriter
import java.nio.file.Paths
import wacc.back_end.IR.StringInfo

object AssemblyWriter {
  val assemblyBuilder = List.newBuilder[String]

  // Writes the assembly file
  def generateAsmFile(ir: (List[StringInfo], List[FuncLabelDef]), filename: String, folder: String = "") = {
    val file = Paths.get(filename).getFileName.toString
    ir._2.map(labelHandler(_)) // Generates the assembly for each label definition  
    val finalAssembly = assemblyBuilder.result()
    assemblyBuilder.clear()
    val writer = new PrintWriter(s"$folder${file.dropRight(5)}.s")
    // Write boilerplate header assembly
    val boilerplate = List(
      ".intel_syntax noprefix",
      ".globl main",
      ".section .rodata",
      "   .int 5",
      ".L.strfalse:",
      "   .asciz \"false\"",
      "   .int 4",
      ".L.strtrue:",
      "   .asciz \"true\"",
      "   .int 45",
      ".L.str_errNull:",
      "   .asciz \"fatal error: null pair dereferenced or freed\\n\"",
      "   .int 27",
      ".L.str_errOutOfMemory:",
      "   .asciz \"fatal error: out of memory\\n\"",
      "   .int 52",
      ".L.str_errOverflow:",
      "   .asciz \"fatal error: integer overflow or underflow occurred\\n\"",
      "   .int 47",
      ".L.str_errBadChar:",
      "   .asciz \"fatal error: int is not ascii character 0-127 \\n\"",
      "   .int 42",
      ".L.str_errOutOfBounds:",
      "   .asciz \"fatal error: array index %d out of bounds\\n\"",
      "   .int 40",
      ".L.str_errDivZero:",
      "   .asciz \"fatal error: division or modulo by zero\\n\""
      )





    boilerplate.foreach(writer.println)
    
    // Write string info directives into ro section
    ir._1.foreach {
      s =>
        writer.println(s"   .int ${s.len}")
        writer.println(s".L.str${s.strCount}:")
        writer.println(s"   .asciz \"${s.string}\" ") 
    }
    // Write assembly instructions section
    writer.println(".text")
    finalAssembly.foreach(writer.println)
    writer.close()
  }
    
  // TODO: rename label handler to label def handler
  // Appends assembly text for given label definition
  def labelHandler(label: FuncLabelDef): Unit = {
    assemblyBuilder += (s"${label.name}:")
    label.instructions.result.map(instructionHandler(_))
  }

  // Appends assembly text for given instruction
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

  // Adding conditional jumps to assembly file
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
