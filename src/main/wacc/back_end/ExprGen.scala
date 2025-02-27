package wacc.back_end
import wacc.front_end._
import wacc.back_end.RegName._
import wacc.back_end.DataWidth._
import wacc.back_end.Cond._
import scala.collection.mutable.Builder
import wacc.back_end.IR.repeatAccessArray
import scala.util.control.Exception.By

// Complete:
// All Ops / Len
// All Literals / Strings

// TODO: 
// Len (Requires Arrays)
// String, and Array Generators need Implementation
// Char err (Bounds)
// overflowErr Instructions (At Bottom)
// divByZeroErr Instructions (At Bottom)

object ExprGen {
  def exprGen(expr: Expr, builder: Builder[Instr, List[Instr]], regNum: Int = 10): (Reg | Imm) = {
    expr match 
      case NullLiteral => Imm(0)
      case IntLiteral(int) => Imm(int)
      case BoolLiteral(bool) => Imm(if bool then 1 else 0)
      case CharLiteral(char) => Imm(char.toInt)
      case op: Operator => {
        opGen(op, regNum, builder)
      }
      case qn: QualifiedName => {
        val offset = Stack.frames.last.identTable(qn)
        val memSize = Stack.typeToSize(qn.t)
        builder += MOV(Reg(regNum, memSize), OffsetAddr(Some(memSize), Reg(Rbp, QWord), offset))
        Reg(regNum, memSize)
      }
      case StringLiteral(string) => stringGen()
      case ArrayElem(qn: QualifiedName, index) => {
        val (memSize, pointer) = repeatAccessArray(qn, index, builder)
        builder += MOV(Reg(regNum, QWord), pointer)
        Reg(regNum, memSize)
      }
  }

  def exprGenRegister(expr: Expr, builder: Builder[Instr, List[Instr]], regNum: Int = 10): Reg = {
    expr match 
      case NullLiteral => builder += MOV(Reg(regNum, QWord), Imm(0)) ; Reg(regNum, QWord)
      case IntLiteral(int) => builder += MOV(Reg(regNum, DWord), Imm(int)) ; Reg(regNum, DWord)
      case BoolLiteral(bool) => builder += MOV(Reg(regNum, Byte), Imm(if bool then 1 else 0)) ; Reg(regNum, Byte)
      case CharLiteral(char) => builder += MOV(Reg(regNum, Byte), Imm(char.toInt)) ; Reg(regNum, Byte)
      case _ => exprGen(expr, builder, regNum).asInstanceOf[Reg]
  }

  def opGen(op: Operator, regNum: Int, builder: Builder[Instr, List[Instr]]): Reg = {
    op match
      case Add(l, r) => arithmeticOp(l, r, regNum, builder, ADD.apply)
      case Sub(l, r) => arithmeticOp(l, r, regNum, builder, SUB.apply)
      case Mul(l, r) => arithmeticNonImm(l, r, regNum, builder, IMUL.apply) 
      case Div(l, r) => divMod(l, r, regNum, Rax, builder)
      case Mod(l, r) => divMod(l, r, regNum, Rdx, builder)
      case Less(l, r) => cmpOp(l, r, regNum, builder,  L)       
      case LessE(l, r) => cmpOp(l, r, regNum, builder,  LE)
      case Greater(l, r) => cmpOp(l, r, regNum, builder,  G)
      case GreaterE(l, r) => cmpOp(l, r, regNum, builder,  GE)
      case Eq(l, r) => cmpOp(l, r, regNum, builder,  E)
      case NotEq(l, r) => cmpOp(l, r, regNum, builder,  NE)
      case And(l, r) => logicOp(l, r, regNum, builder, AND.apply)
      case Or(l, r) => logicOp(l, r, regNum, builder, OR.apply)
      case Ord(x) => {exprGenRegister(x, builder, regNum)}

      case Neg(x) => 
        exprGenRegister(x, builder, regNum)
        builder += (NEG(Reg(regNum, DWord)))
        overflowErr(regNum)
        Reg(regNum, DWord)

      case Not(x) => 
        exprGenRegister(x, builder, regNum)
        builder.addAll(
          List(
            CMP(Reg(regNum, Byte), Imm(0)),
            XOR(Reg(regNum, Byte), Reg(regNum, Byte)),
            SETCond(E, Reg(regNum, Byte))
            )
        )
        Reg(regNum, Byte)
  

      case Chr(x) => 
        exprGenRegister(x, builder, regNum) 
        // Need to check that is 0 <= x <= 127

      case Len(x) => ???
  }

  def binOpHelper(l: Expr, r: Expr, regNum: Int, builder: Builder[Instr, List[Instr]]) = {
    if regNum == 14 then {
      exprGenRegister(r, builder, 14)
      builder += (PUSH(Reg(R14, QWord)))
      exprGenRegister(l, builder, 14)
      builder += (POP(Reg(R15, QWord)))
    } else {
      exprGenRegister(l, builder, regNum)
      exprGenRegister(r, builder, regNum+1)
    }
  }

  def arithmeticOp(l: Expr, r: Expr, regNum: Int, builder: Builder[Instr, List[Instr]], op: FullOps => Instr) = {
    r match {
      case IntLiteral(int) =>
        exprGenRegister(l, builder, regNum) 
        builder += op(Reg(regNum, DWord), Imm(int))
        overflowErr(regNum)
        Reg(regNum, DWord)
      case _ => arithmeticNonImm(l, r, regNum, builder, op)

    }
  }

  def arithmeticNonImm(l: Expr, r: Expr, regNum: Int, builder: Builder[Instr, List[Instr]], op: LocationOps => Instr) = {
    binOpHelper(l, r, regNum, builder)
    builder += op(Reg(regNum, DWord), Reg(regNum + 1, DWord))
    overflowErr(regNum)
    Reg(regNum, DWord)
  }

  def logicOp(l: Expr, r: Expr, regNum: Int, builder: Builder[Instr, List[Instr]], op: FullOps => Instr) = {
    r match {
      case BoolLiteral(bool) =>
        exprGenRegister(l, builder, regNum)
        builder += op(Reg(regNum, Byte), Imm(if bool then 1 else 0))
      case _ =>
        binOpHelper(l, r, regNum, builder)
        builder += op(Reg(regNum, Byte), Reg(regNum + 1, Byte))
    }
    Reg(regNum, Byte)
  }

  def cmpOp(l: Expr, r: Expr, regNum: Int, builder: Builder[Instr, List[Instr]], cond: Cond) = {
    binOpHelper(l,r, regNum, builder)
    builder += CMP(Reg(regNum, DWord), Reg(regNum+1, DWord))
    SETCond(cond, Reg(regNum, Byte))
    Reg(regNum, Byte)
  }

  def divMod(l: Expr, r: Expr, regNum: Int, reg: RegName, builder: Builder[Instr, List[Instr]]) = {
    // Reference doesn't save Rax, Rdx?

    builder.addAll(
      List(
        PUSH(Reg(Rax, QWord)),
        PUSH(Reg(Rdx, QWord))
        )
    )
    exprGenRegister(l, builder, regNum) 
    builder += MOV(Reg(Rax, DWord), Reg(regNum, DWord))
    exprGenRegister(r, builder, regNum) 
    divByZeroErr(regNum) 
    builder.addAll(
      List(CDQ,
      IDIV(Reg(regNum, DWord)),
      MOV(Reg(regNum, DWord), Reg(reg, DWord)),
      POP(Reg(Rdx, QWord)),
      POP(Reg(Rax, QWord)))
    )
    Reg(reg, DWord)
    // So doesn't need to push em
  }

  def overflowErr(regNum: Int): List[Instr] = Nil
  def divByZeroErr(regNum: Int): List[Instr] = Nil

  def stringGen() = ???
  def arrayElemGen() = ???
}
