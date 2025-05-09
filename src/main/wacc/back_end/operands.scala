package wacc.back_end
import scala.language.implicitConversions
import scala.collection.mutable.{Builder}

enum RegName:
  case Rax, Rbx, Rcx, Rdx, Rsi, Rdi, Rsp, Rbp, Rip, R8, R9, R10, R11, R12, R13, R14, R15

enum DataWidth:
  case Byte, Word, DWord, QWord

enum MemOpModifier:
  case BytePtr, WordPtr, DWordPtr, QWordPtr
  override def toString(): String = { 
    this match
      case BytePtr => "byte ptr"
      case WordPtr => "word ptr"
      case DWordPtr => "dword ptr"
      case QWordPtr => "qword ptr"
  }

// Operand is a location or immediate
sealed trait Operand 
sealed trait Location extends Operand {
  override def toString: String = (this: @unchecked) match {
    case OffsetAddr(memOpModifier, reg1, disp) if disp > 0 => s"$memOpModifier [$reg1 + $disp]" 
    case OffsetAddr(memOpModifier, reg1, disp) if disp == 0 => s"$memOpModifier [$reg1]" 
    case OffsetAddr(memOpModifier, reg1, disp) => s"$memOpModifier [$reg1 - ${-disp}]"
    case StringAddr(strCount, reg) => s"[$reg + .L.str${strCount}]"
    case RegScale(memOpModifier, reg1, scale, reg2) => s"$memOpModifier [$reg1 + $scale*$reg2]"
  }

}
case class Reg(num: RegName, dataWidth: DataWidth) extends Location {
  override def toString(): String = {
    num match
      case RegName.Rax => regABCD(dataWidth, "a")
      case RegName.Rbx => regABCD(dataWidth, "b")
      case RegName.Rcx => regABCD(dataWidth, "c")
      case RegName.Rdx => regABCD(dataWidth, "d")
      case RegName.Rsi => regSpBpSiDi(dataWidth, "si")
      case RegName.Rdi => regSpBpSiDi(dataWidth, "di")
      case RegName.Rsp => regSpBpSiDi(dataWidth, "sp")
      case RegName.Rbp => regSpBpSiDi(dataWidth, "bp")
      case RegName.R8 => regNums(dataWidth, "r8")
      case RegName.R9 => regNums(dataWidth, "r9")
      case RegName.R10 => regNums(dataWidth, "r10")
      case RegName.R11 => regNums(dataWidth, "r11")
      case RegName.R12 => regNums(dataWidth, "r12")
      case RegName.R13 => regNums(dataWidth, "r13")
      case RegName.R14 => regNums(dataWidth, "r14")
      case RegName.R15 => regNums(dataWidth, "r15")
      case RegName.Rip => "rip"
  }
}

  def regABCD(dataWidth: DataWidth, regString: String): String = {
    dataWidth match
      case DataWidth.Byte => regString + "l"
      case DataWidth.Word => regString + "x"
      case DataWidth.DWord => "e" + regString + "x"
      case DataWidth.QWord => "r" + regString + "x"
  }

  def regSpBpSiDi(dataWidth: DataWidth, regString: String): String = {
    dataWidth match
      case DataWidth.Byte => regString + "l"
      case DataWidth.Word => regString
      case DataWidth.DWord => "e" + regString
      case DataWidth.QWord => "r" + regString
  }

  def regNums(dataWidth: DataWidth, regString: String): String = {
    dataWidth match
      case DataWidth.Byte => regString + "b"
      case DataWidth.Word => regString + "w"
      case DataWidth.DWord => regString + "d"
      case DataWidth.QWord => regString
  }


case class Imm(value: BigInt) extends Operand {
  override def toString(): String = value.toString()
}
case class Label(name: String) extends Operand {
  override def toString(): String = name.toString()
}

sealed trait MemAddr extends Location

case class OffsetAddr(modifer: MemOpModifier, reg1: Reg, disp: Int = 0) extends MemAddr 
case class RegScale(modifer: MemOpModifier, reg1: Reg, scale: Int, reg2: Reg) extends MemAddr
case class StringAddr(strCount: String, reg: Reg = Reg(RegName.Rip, DataWidth.QWord)) extends MemAddr

object MemOpModifier {
  implicit def intToMemOpModifier(size: Int): MemOpModifier = {
    size match
      case 1 => BytePtr
      case 2 => WordPtr
      case 4 => DWordPtr
      case 8 => QWordPtr
  }
  implicit def dataWidthToModifier(dataWidth: DataWidth): MemOpModifier = {
    dataWidth match
      case DataWidth.Byte => BytePtr 
      case DataWidth.Word => WordPtr
      case DataWidth.DWord => DWordPtr
      case DataWidth.QWord => QWordPtr
  }
}

object DataWidth {
  implicit def intToDataWidth(size: Int): DataWidth = {
    size match
      case 1 => Byte
      case 2 => Word
      case 4 => DWord
      case 8 => QWord
  }

}

object RegName {
  implicit val intToRegMap: Map[Int, RegName] = Map(
    10 -> RegName.R10,
    11 -> RegName.R11,
    12 -> RegName.R12,
    13 -> RegName.R13,
    14 -> RegName.R14,
    15 -> RegName.R15
  )

  val regToIntMap: Map[RegName, Int] = intToRegMap.map(_.swap)

  implicit def intToRegName(regNum: Int): RegName = 
    intToRegMap.getOrElse(regNum, throw new IllegalArgumentException(s"Invalid register number: $regNum"))
  
  implicit def regToInt(regNum: RegName): Int =
    regToIntMap.getOrElse(regNum, throw new IllegalArgumentException(s"Invalid register name: $regNum"))
}

object Reg {
  implicit def regNameToReg(regName: RegName): Reg = Reg(regName, DataWidth.QWord)
}

case class FuncLabelDef(
  name: String,
  instructions: Builder[Instr, List[Instr]]
)