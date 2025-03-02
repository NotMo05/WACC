package wacc.back_end
import wacc.front_end._
import wacc.back_end.RegName._
import wacc.back_end.DataWidth._
import wacc.back_end.Cond._
import scala.collection.mutable.Builder
import wacc.back_end.IR.repeatAccessArray
import wacc.front_end.semantic.getExprType
import wacc.back_end.Stack.typeToSize

/**
 * The `ExprGen` object is responsible for generating assembly instructions
 * for various expressions in the WACC language. It provides methods to handle
 * different types of expressions, including literals, operators, and qualified names.
 * The generated instructions are added to an assembly builder, which is used to 
 * construct the final assembly code.
 */

object ExprGen {
  // Generates and appends any required assembly IR for evaluating the given expression
  def exprGen(expr: Expr, asmBuilder: Builder[Instr, List[Instr]], regNum: Int = 10): (Reg | Imm) = {
    (expr: @unchecked) match 
      case NullLiteral => Imm(0)
      case IntLiteral(int) => Imm(int)
      case BoolLiteral(bool) => Imm(if bool then 1 else 0)
      case CharLiteral(char) => Imm(char.toInt)
      case op: Operator => {
        opGen(op, regNum, asmBuilder)
      }
      case qn: QualifiedName => {
        val offset = Stack.frames.last.identTable(qn)
        val dataWidth = Stack.typeToSize(qn.t)
        asmBuilder += MOV(Reg(regNum, dataWidth), OffsetAddr(dataWidth, Reg(Rbp, QWord), offset))
        Reg(regNum, dataWidth)
      }
      case ArrayElem(qn: QualifiedName, index) => {
        val (dataWidth, pointer) = repeatAccessArray(qn, index, asmBuilder)
        asmBuilder += MOV(Reg(regNum, dataWidth), pointer)
        Reg(regNum, dataWidth)
      }
      case StringLiteral(string) => {
        val n = IR.strMap(string).toString()
        asmBuilder += LEA(Reg(regNum, QWord), StringAddr(n))
        Reg(regNum, QWord)
      }
  }

  // Generates assembly instructions for an expression and stores the result in a register
  def exprGenRegister(expr: Expr, asmBuilder: Builder[Instr, List[Instr]], regNum: Int = 10): Reg = {
    expr match 
      case NullLiteral => asmBuilder += MOV(Reg(regNum, QWord), Imm(0)) ; Reg(regNum, QWord)
      case IntLiteral(int) => asmBuilder += MOV(Reg(regNum, DWord), Imm(int)) ; Reg(regNum, DWord)
      case BoolLiteral(bool) => asmBuilder += MOV(Reg(regNum, Byte), Imm(if bool then 1 else 0)) ; Reg(regNum, Byte)
      case CharLiteral(char) => asmBuilder += MOV(Reg(regNum, Byte), Imm(char.toInt)) ; Reg(regNum, Byte)
      case _ => exprGen(expr, asmBuilder, regNum).asInstanceOf[Reg]
  }

  // Generates assembly code for a given operator and returns the result register
  def opGen(op: Operator, regNum: Int, asmBuilder: Builder[Instr, List[Instr]]): Reg = {
    op match
      case Add(l, r) => arithmeticOp(l, r, regNum, asmBuilder, ADD.apply)
      case Sub(l, r) => arithmeticOp(l, r, regNum, asmBuilder, SUB.apply)
      case Mul(l, r) => arithmeticNonImm(l, r, regNum, asmBuilder, IMUL.apply) 
      case Div(l, r) => divMod(l, r, regNum, Rax, asmBuilder)
      case Mod(l, r) => divMod(l, r, regNum, Rdx, asmBuilder)
      case Less(l, r) => cmpOp(l, r, regNum, asmBuilder,  L)  
      case LessE(l, r) => cmpOp(l, r, regNum, asmBuilder,  LE)
      case Greater(l, r) => cmpOp(l, r, regNum, asmBuilder,  G)
      case GreaterE(l, r) => cmpOp(l, r, regNum, asmBuilder,  GE)
      case Eq(l, r) => cmpOp(l, r, regNum, asmBuilder,  E)
      case NotEq(l, r) => cmpOp(l, r, regNum, asmBuilder,  NE)
      case And(l, r) => logicOp(l, r, regNum, asmBuilder, AND.apply)
      case Or(l, r) => logicOp(l, r, regNum, asmBuilder, OR.apply)
      
      case Ord(x) => {
        asmBuilder += MOVZX(Reg(regNum, DWord), exprGenRegister(x, asmBuilder, regNum+1))
        Reg(regNum, DWord)
      }

      case Neg(x) => {
        exprGenRegister(x, asmBuilder, regNum)
        asmBuilder += (NEG(Reg(regNum, DWord)))
        asmBuilder += JCond(Cond.O, Label("_errOverflow"))
        Reg(regNum, DWord)
      }

      case Not(x) => {
        exprGenRegister(x, asmBuilder, regNum)
        asmBuilder.addAll(
          List(
            CMP(Reg(regNum, Byte), Imm(1)),
            SETCond(NE, Reg(regNum, Byte))
            )
        )
        Reg(regNum, Byte)
        }

      case Chr(x) => {  
        exprGenRegister(x, asmBuilder, regNum)
        asmBuilder.addAll(
          List(
            CMP(Reg(R10, DWord), Imm(127)),
            JCond(G, Label("_errBadChar")),
            CMP(Reg(R10, DWord), Imm(0)),
            JCond(L, Label("_errBadChar")),
          )
        )
        Reg(R10, Byte)
      }

      case Len(x) => {
        exprGen(x, asmBuilder)
        asmBuilder += MOV(Reg(R10, DWord), OffsetAddr(MemOpModifier.DWordPtr, Reg(R10, QWord), -4))
        Reg(R10, DWord)
      }
  }

  // Helper function to generate assembly code for binary operations
  def binOpHelper(l: Expr, r: Expr, regNum: Int, asmBuilder: Builder[Instr, List[Instr]]) = {
    if regNum == 14 then {
      exprGenRegister(r, asmBuilder, 14)
      asmBuilder += (PUSH(Reg(R14, QWord)))
      exprGenRegister(l, asmBuilder, 14)
      asmBuilder += (POP(Reg(R15, QWord)))
    } else {
      exprGenRegister(l, asmBuilder, regNum)
      exprGenRegister(r, asmBuilder, regNum+1)
    }
  }

  // Generates assembly instructions for an arithmetic operation between two expressions
  def arithmeticOp(l: Expr, r: Expr, regNum: Int, asmBuilder: Builder[Instr, List[Instr]], op: FullOps => Instr) = {
    r match {
      case IntLiteral(int) =>
        exprGenRegister(l, asmBuilder, regNum) 
        asmBuilder += op(Reg(regNum, DWord), Imm(int))
        asmBuilder += JCond(Cond.O, Label("_errOverflow"))
        Reg(regNum, DWord)
      case _ => arithmeticNonImm(l, r, regNum, asmBuilder, op)

    }
  }

  // Generates assembly instructions for non-immediate arithmetic operations between two expressions
  def arithmeticNonImm(l: Expr, r: Expr, regNum: Int, asmBuilder: Builder[Instr, List[Instr]], op: LocationOps => Instr) = {
    binOpHelper(l, r, regNum, asmBuilder)
    asmBuilder += op(Reg(regNum, DWord), Reg(regNum + 1, DWord))
    asmBuilder += JCond(Cond.O, Label("_errOverflow"))
    Reg(regNum, DWord)
  }

  // Generates assembly instructions for logical operations between two expressions.
  def logicOp(l: Expr, r: Expr, regNum: Int, asmBuilder: Builder[Instr, List[Instr]], op: FullOps => Instr) = {
    r match {
      case BoolLiteral(bool) =>
        exprGenRegister(l, asmBuilder, regNum)
        asmBuilder += op(Reg(regNum, Byte), Imm(if bool then 1 else 0))
      case _ =>
        binOpHelper(l, r, regNum, asmBuilder)
        asmBuilder += op(Reg(regNum, Byte), Reg(regNum + 1, Byte))
    }
    Reg(regNum, Byte)
  }

  // Compares two expressions and generates assembly instructions based on the comparison result
  def cmpOp(l: Expr, r: Expr, regNum: Int, asmBuilder: Builder[Instr, List[Instr]], cond: Cond) = {
    val dataWidth = typeToSize(getExprType(l).get)
    binOpHelper(l,r, regNum, asmBuilder)
    asmBuilder += CMP(Reg(regNum, dataWidth), Reg(regNum+1, dataWidth))
    asmBuilder += SETCond(cond, Reg(regNum, Byte))
    Reg(regNum, Byte)
  }

  // Generates assembly instructions for division and modulus operations
  def divMod(l: Expr, r: Expr, regNum: Int, reg: RegName, asmBuilder: Builder[Instr, List[Instr]]) = {
    asmBuilder.addAll(
      List(
        PUSH(Reg(Rax, QWord)),
        PUSH(Reg(Rdx, QWord))
      )
    )

    exprGenRegister(l, asmBuilder, regNum) 
    asmBuilder += MOV(Reg(Rax, DWord), Reg(regNum, DWord))
    asmBuilder += CMP(exprGenRegister(r, asmBuilder, regNum), Imm(0))
    asmBuilder += JCond(Cond.E, Label("_errDivZero"))
    asmBuilder.addAll(
      List(CDQ,
      IDIV(Reg(regNum, DWord)),
      MOV(Reg(regNum, DWord), Reg(reg, DWord)),
      POP(Reg(Rdx, QWord)),
      POP(Reg(Rax, QWord)))
    )
    
    Reg(regNum, DWord)
  }
}
