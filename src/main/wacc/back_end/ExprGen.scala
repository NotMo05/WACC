package wacc.back_end
import wacc.front_end._
import wacc.back_end.RegName._
import wacc.back_end.DataWidth._
import wacc.back_end.Cond._

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
  def exprGen(expr: Expr, regNum: Int = 10): List[Instr] = {
    expr match 
      case NullLiteral => List(MOV(Reg(regNum, QWord), Imm(0)))
      case IntLiteral(int) => List(MOV(Reg(regNum, DWord), Imm(int)))
      case BoolLiteral(bool) => List(MOV(Reg(regNum, Byte), Imm(if bool then 1 else 0)))
      case CharLiteral(char) => List(MOV(Reg(regNum, Byte), Imm(char.toInt)))
      case op: Operator => opGen(op, regNum)
      case qn: QualifiedName => {
        val offset = Stack.frames.last.identTable(qn)
        val memOp = Stack.intToMemOpModifier.get(Stack.typeToSize(qn.t))
        List(MOV(Reg(regNum), DisplAddr(memOp, Reg(Rbp), offset)))
      }
      case StringLiteral(string) => stringGen()
      case ArrayElem(arrayName, index) => arrayElemGen()
  }

  def opGen(op: Operator, regNum: Int) = {
    op match
      case Add(l, r) => arithmeticOp(l, r, regNum, ADD.apply)
      case Sub(l, r) => arithmeticOp(l, r, regNum, SUB.apply)
      case Mul(l, r) => arithmeticNonImm(l, r, regNum, IMUL.apply) 
      case Div(l, r) => divMod(l, r, regNum, Rax)
      case Mod(l, r) => divMod(l, r, regNum, Rdx)
      case Less(l, r) => cmpOp(l, r, regNum, L)       
      case LessE(l, r) => cmpOp(l, r, regNum, LE)
      case Greater(l, r) => cmpOp(l, r, regNum, G)
      case GreaterE(l, r) => cmpOp(l, r, regNum, GE)
      case Eq(l, r) => cmpOp(l, r, regNum, E)
      case NotEq(l, r) => cmpOp(l, r, regNum, NE)
      case And(l, r) => logicOp(l, r, regNum, AND.apply)
      case Or(l, r) => logicOp(l, r, regNum, OR.apply)
      case Ord(x) => exprGen(x, regNum)

      case Neg(x) => 
        exprGen(x, regNum) ++
        List(NEG(Reg(regNum))) ++
        overflowErr(regNum)

      case Not(x) => 
        exprGen(x, regNum) ++
        List(CMP(Reg(regNum), Imm(0)),
          XOR(Reg(regNum), Reg(regNum)),
          SETCond(E, Reg(regNum)))

      case Chr(x) => 
        exprGen(x, regNum) 
        // Need to check that is 0 <= x <= 127

      case Len(x) => ???
  }

  def binOpHelper(l: Expr, r: Expr, regNum: Int): List[Instr] = {
    if regNum == 14 then {
      exprGen(r, 14) ++
      List(PUSH(Reg(R14))) ++
      exprGen(l, 14) ++
      List(POP(Reg(R15)))
    } else {
      exprGen(l, regNum) ++ 
      exprGen(r, regNum+1)
    }
  }

  def arithmeticOp(l: Expr, r: Expr, regNum: Int, op: FullOps => Instr) : List[Instr] = {
    r match {
      case IntLiteral(int) =>
        exprGen(l, regNum) ++
        List(op(Reg(regNum), Imm(int))) ++
        overflowErr(regNum)
      case _ => arithmeticNonImm(l, r, regNum, op)

    }
  }

  def arithmeticNonImm(l: Expr, r: Expr, regNum: Int, op: LocationOps => Instr) = {
    binOpHelper(l, r, regNum) ++
    List(op(Reg(regNum), Reg(regNum + 1))) ++
    overflowErr(regNum)
  }

  def logicOp(l: Expr, r: Expr, regNum: Int, op: FullOps => Instr) : List[Instr] = {
    r match {
      case BoolLiteral(bool) =>
        exprGen(l, regNum) ++
        List(op(Reg(regNum), Imm(if bool then 1 else 0)))
      case _ =>
        binOpHelper(l, r, regNum) ++
        List(op(Reg(regNum), Reg(regNum + 1)))
    }
  }

  def cmpOp(l: Expr, r: Expr, regNum: Int, cond: Cond) = {
    binOpHelper(l,r, regNum) ++
    List(CMP(Reg(regNum), Reg(regNum+1)),
    SETCond(cond, Reg(regNum)))
  }

  def divMod(l: Expr, r: Expr, regNum: Int, reg: RegName) = {
    // Reference doesn't save Rax, Rdx?
    List(PUSH(Reg(Rax)),
    PUSH(Reg(Rdx)))++
    exprGen(l, regNum) ++
    List(MOV(Reg(Rax), Reg(regNum))) ++
    exprGen(r, regNum) ++
    divByZeroErr(regNum) ++
    List(CDQ,
    IDIV(Reg(regNum)),
    MOV(Reg(regNum), Reg(reg)),
    POP(Reg(Rdx)),
    POP(Reg(Rax)))
    // So doesn't need to push em
  }

  def overflowErr(regNum: Int): List[Instr] = Nil
  def divByZeroErr(regNum: Int): List[Instr] = Nil

  def stringGen() = ???
  def arrayElemGen() = ???
}

