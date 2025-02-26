package wacc.back_end
import wacc.front_end._
import wacc.back_end.RegName._
import wacc.back_end.DataWidth._
import wacc.back_end.Cond._
import scala.collection.mutable.Builder

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
  def exprGen(expr: Expr, builder: Builder[Instr, List[Instr]], regNum: Int = 10): Unit = {
    expr match 
      case NullLiteral => builder += MOV(Reg(regNum, QWord), Imm(0))
      case IntLiteral(int) => builder += MOV(Reg(regNum, DWord), Imm(int))
      case BoolLiteral(bool) => builder += MOV(Reg(regNum, Byte), Imm(if bool then 1 else 0))
      case CharLiteral(char) => builder += MOV(Reg(regNum, Byte), Imm(char.toInt))
      case op: Operator => opGen(op, regNum, builder)
      case qn: QualifiedName => {
        val offset = Stack.frames.last.identTable(qn)
        builder += MOV(Reg(regNum), OffsetAddr(Some(Stack.typeToSize(qn.t)), Reg(Rbp), offset))
      }
      case StringLiteral(string) => stringGen()
      case ArrayElem(arrayName, index) => arrayElemGen()
  }

  def opGen(op: Operator, regNum: Int, builder: Builder[Instr, List[Instr]]) = {
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
      case Ord(x) => exprGen(x, builder, regNum)

      case Neg(x) => 
        exprGen(x, builder, regNum)
        builder += (NEG(Reg(regNum)))
        overflowErr(regNum)

      case Not(x) => 
        exprGen(x, builder, regNum)
        builder.addAll(
          List(
            CMP(Reg(regNum), Imm(0)),
            XOR(Reg(regNum), Reg(regNum)),
            SETCond(E, Reg(regNum))
            )
        )

      case Chr(x) => 
        exprGen(x, builder, regNum) 
        // Need to check that is 0 <= x <= 127

      case Len(x) => ???
  }

  def binOpHelper(l: Expr, r: Expr, regNum: Int, builder: Builder[Instr, List[Instr]]) = {
    if regNum == 14 then {
      exprGen(r, builder, 14)
      builder += (PUSH(Reg(R14)))
      exprGen(l, builder, 14)
      builder += (POP(Reg(R15)))
    } else {
      exprGen(l, builder, regNum)
      exprGen(r, builder, regNum+1)
    }
  }

  def arithmeticOp(l: Expr, r: Expr, regNum: Int, builder: Builder[Instr, List[Instr]], op: FullOps => Instr) = {
    r match {
      case IntLiteral(int) =>
        exprGen(l, builder, regNum) 
        builder += op(Reg(regNum), Imm(int))
        overflowErr(regNum)
      case _ => arithmeticNonImm(l, r, regNum, builder, op)

    }
  }

  def arithmeticNonImm(l: Expr, r: Expr, regNum: Int, builder: Builder[Instr, List[Instr]], op: LocationOps => Instr) = {
    binOpHelper(l, r, regNum, builder)
    builder += op(Reg(regNum), Reg(regNum + 1))
    overflowErr(regNum)
  }

  def logicOp(l: Expr, r: Expr, regNum: Int, builder: Builder[Instr, List[Instr]], op: FullOps => Instr) = {
    r match {
      case BoolLiteral(bool) =>
        exprGen(l, builder, regNum)
        builder += op(Reg(regNum), Imm(if bool then 1 else 0))
      case _ =>
        binOpHelper(l, r, regNum, builder)
        builder += op(Reg(regNum), Reg(regNum + 1))
    }
  }

  def cmpOp(l: Expr, r: Expr, regNum: Int, builder: Builder[Instr, List[Instr]], cond: Cond) = {
    binOpHelper(l,r, regNum, builder)
    builder += CMP(Reg(regNum), Reg(regNum+1))
    SETCond(cond, Reg(regNum))
  }

  def divMod(l: Expr, r: Expr, regNum: Int, reg: RegName, builder: Builder[Instr, List[Instr]]) = {
    // Reference doesn't save Rax, Rdx?

    builder.addAll(
      List(
        PUSH(Reg(Rax)),
        PUSH(Reg(Rdx))
        )
    )
    exprGen(l, builder, regNum) 
    builder += MOV(Reg(Rax), Reg(regNum))
    exprGen(r, builder, regNum) 
    divByZeroErr(regNum) 
    builder.addAll(
      List(CDQ,
      IDIV(Reg(regNum)),
      MOV(Reg(regNum), Reg(reg)),
      POP(Reg(Rdx)),
      POP(Reg(Rax)))
    )
    // So doesn't need to push em
  }

  def overflowErr(regNum: Int): List[Instr] = Nil
  def divByZeroErr(regNum: Int): List[Instr] = Nil

  def stringGen() = ???
  def arrayElemGen() = ???
}

