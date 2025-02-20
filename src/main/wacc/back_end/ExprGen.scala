package wacc.back_end
import wacc.front_end._
import wacc.back_end._
import wacc.back_end.RegName._
import wacc.back_end.DataWidth._
import wacc.back_end.Cond._

// If we reserve R10, R11 for Expr Gen we dont need to pus/pop at begin/end


// TODO: Div/Mod (Divide by 0 err), Strings, Idents, Arrays
// Char err (Bounds)
// overflowErr Instructions (At Bottom)

object ExprGen {
  def exprGen(expr: Expr): List[Instr] = {
    expr match
      case NullLiteral => List(MOV(Reg(R10, QWord), Imm(0))) // This is what reference compiler does?
      case IntLiteral(int) => List(MOV(Reg(R10, DWord), Imm(int)))
      case BoolLiteral(bool) => List(MOV(Reg(R10, Byte), Imm(if bool then 1 else 0)))
      case CharLiteral(char) => List(MOV(Reg(R10, Byte), Imm(char.toInt)))
      case op: Operator => opGen(op)
      case StringLiteral(string) => ???
      case qn: QualifiedName => ???
      case ArrayElem(arrayName, index) => ???
  }

  def opGen(op: Operator) = {
    op match
      case Div(l, r) => ???
      case Mod(l, r) => ???
      case Len(x) => ???

      case Mul(l, r) => {
        binaryOpHelper(l,r) ++
        List(MUL(Reg(R10), Reg(R11))) ++
        overflowErr(R10)       
      }

      case Add(l, r) => {
        binaryOpHelper(l,r) ++
        List(ADD(Reg(R10), Reg(R11))) ++
        overflowErr(R10)       
      }
      case Sub(l, r) => {
        binaryOpHelper(l,r) ++
        List(SUB(Reg(R10), Reg(R11))) ++
        overflowErr(R10)       
      }

      case Less(l, r) => {
        binaryOpHelper(l,r) ++
        cmpOpHelper(L)       
      }

      case LessE(l, r) => {
        binaryOpHelper(l,r) ++
        cmpOpHelper(LE)
      }

      case Greater(l, r) => {
        binaryOpHelper(l,r) ++
        cmpOpHelper(G)        
      }

      case GreaterE(l, r) => {
        binaryOpHelper(l,r) ++
        cmpOpHelper(GE)
        
      }

      case Eq(l, r) => {
        binaryOpHelper(l,r) ++
        cmpOpHelper(E)        
      }
      case NotEq(l, r) => {
        binaryOpHelper(l,r) ++
        cmpOpHelper(NE)
      }

      case And(l, r) => {
        binaryOpHelper(l,r) :+
        AND(Reg(R10), Reg(R11))
      }

      case Or(l, r) => 
        binaryOpHelper(l,r) :+
        OR(Reg(R10), Reg(R11))
    
      case Neg(x) => {
        exprGen(x) ++
        List(NEG(Reg(R10))) ++
        overflowErr(R10)
      }

      case Not(x) => {
        exprGen(x) ++
        List(CMP(Reg(R10), Imm(0)),
          XOR(Reg(R10), Reg(R10)),
          SETCond(E, Reg(R10)))
      }

      case Chr(x) => {
        exprGen(x) 
        // Need to check that is 0 <= x <= 127
      }

      case Ord(x) => exprGen(x)
  }

  def binaryOpHelper(l: Expr, r: Expr): List[Instr] =
    exprGen(l) ++ // Figures out what l is and stores in R10
    List(PUSH(Reg(R10))) ++ 
    exprGen(r) ++ // Figures out what r is and stores in R10
    List(POP(Reg(R11)))
    // R11 now stores l, and R10 stores r 
  
  def cmpOpHelper(cond: Cond) =
    List(CMP(Reg(R10), Reg(R11)),
    SETCond(cond, Reg(R10)))

  def overflowErr(reg: RegName): List[Instr] = {
    Nil
  }
}

