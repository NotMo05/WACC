package wacc.back_end

enum RegName:
  case Rax, Rbx, Rcx, Rdx, Rsi, Rdi, Rsp, Rbp, R8, R9, R10, R11, R12, R13, R14, R15

enum DataWidth:
  case Byte, Word, DWord, QWord

enum MemOpModifier:
  case BytePtr, WordPtr, DWordPtr, QWordPtr

sealed trait Operand
sealed trait Location extends Operand
case class Reg(num: RegName, dataWidth: DataWidth = DataWidth.QWord) extends Location
case class Imm(value: BigInt) extends Operand
case class Label(name: String) extends Operand

sealed trait MemAddr extends Location
case class BaseAddr(modifer: Option[MemOpModifier], reg: Reg) extends MemAddr
case class IndexedAddr(modifer: Option[MemOpModifier], reg1: Reg, reg2: Reg) extends MemAddr
case class DisplAddr(modifer: Option[MemOpModifier], reg1: Reg, disp: Int) extends MemAddr
case class RegScale(modifer: Option[MemOpModifier], reg1: Reg, scale: Int, reg2: Reg) extends MemAddr
case class RegScaleImm(modifer: Option[MemOpModifier], reg1: Reg, scale: Int, reg2: Reg, imm: Int) extends MemAddr
case class ScaleImm(modifer: Option[MemOpModifier], reg: Reg, scale: Int, imm: Int) extends MemAddr

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
  
  implicit def RegToInt(regNum: RegName): Int =
    regToIntMap.getOrElse(regNum, throw new IllegalArgumentException(s"Invalid register name: $regNum"))
}
