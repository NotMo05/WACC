package wacc.back_end

import instrument._
import Cond._

object AssemblyWriter {
  val finalAssembly = List.newBuilder[String]

  def generateAsmFile(ir: (List[Section], List[LabelDef])) = {
    ir._2.map(labelHandler(_))
    val finalA = finalAssembly.result()
    finalA.foreach(println)
    
  }

  def labelHandler(labelDef: LabelDef): Unit = {
    labelDef match 
      case FuncLabelDef(name, instructions, localLabelDefs) => {
        finalAssembly += (s"$name:")
        instructions.result.map(instructionHandler(_))
        localLabelDefs.result.map(labelHandler(_))
      }
      case wacc.back_end.LocalLabelDef(name, instructions) => {
        finalAssembly += (s"$name:")
        instructions.result.map(instructionHandler(_))
      }
  }


  def instructionHandler(instr: Instr) = {
    finalAssembly += (
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
        case SETCond(cond, op) =>
          cond match
            case E =>  s"se $op"
            case NE => s"sne $op"
            case L =>  s"sl $op"
            case LE => s"sle $op"
            case G =>  s"sg $op"
            case GE => s"sge $op"
            case A =>  s"sa $op"
            case AE => s"sae $op"
            case B =>  s"sb $op"
            case BE => s"sbe $op"
            case S =>  s"ss $op"
            case NS => s"sns $op"
            case O =>  s"so $op"
            case NO => s"sno $op"
            case P =>  s"sp $op"
            case NP => s"snp $op" 
        case RET() => s"ret "
        case CALL(op) => s"call $op"
        case CDQ => "cdq "
        case JCond(cond, label) => 
          cond match
            case E => s"je $label"
            case NE => s"jne $label"
            case L => s"jl $label"
            case LE => s"jle $label"
            case G => s"jg $label"
            case GE => s"jge $label"
            case A => s"ja $label"
            case AE => s"jae $label"
            case B => s"jb $label"
            case BE => s"jbe $label"
            case S => s"js $label"
            case NS => s"jns $label"
            case O => s"jo $label"
            case NO => s"jno $label"
            case P => s"jp $label"
            case NP => s"jnp $label"
    )
  }
}
