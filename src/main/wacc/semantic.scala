
package wacc
import scala.collection.mutable
// def semParse(prog: Prog) = {
//   validSemFuncs(prog.funcs) && validSemStmts(prog.main)


object semantic {

  val symbolTable: mutable.Map[String, Type] = mutable.Map()

  def getRValueType(rValue: RValue): Type = {
    rValue match
      case expr: Expr => getExprType(expr)
      case Call(ident, _) => getExprType(ident)
      case ArrayLiter(elems) => getExprType(elems.head) // This needs checking to make sure all elems same type
      case NewPair(fst, snd) => newPairHandle(fst, snd)
      case Fst(lValue) => pairElemHandle(lValue).t1.asInstanceOf[Type]
      case Snd(lValue) => pairElemHandle(lValue).t2.asInstanceOf[Type]
  }
  
  def getLValueType(lValue: LValue): Type = {
    lValue match
      case ident: Ident => getExprType(ident)
      case ArrayElem(arrayName, index) =>  ??? // This changes based on length of index
      case Fst(lValue) => pairElemHandle(lValue).t1.asInstanceOf[Type]
      case Snd(lValue) => pairElemHandle(lValue).t2.asInstanceOf[Type]
  }

  def getExprType(expr: Expr): Type = {
    expr match 
      case op: Operator => validOpArgs(op)
      case int: IntLiteral => IntType
      case bool: BoolLiteral => BoolType
      case string: StringLiteral => StringType
      case char: CharLiteral => CharType
      case ArrayElem(arrayName, _) => getExprType(arrayName)
      case Ident(name) => symbolTable.getOrElse(name, throw new Exception(s"Undeclared identifier: $name"))
      case NullLiteral => ??? // Error?
  }


  private def exprsMatchType(expr1: Expr, expr2: Expr, t: Type) = {
    getExprType(expr1) == t & getExprType(expr2) == t
  }

  def validOpArgs(op: Operator) = {
    op match 
      case Mul(l,r) if exprsMatchType(l, r, IntType) => IntType
      case Div(l,r) if exprsMatchType(l, r, IntType) => IntType
      case Mod(l,r) if exprsMatchType(l, r, IntType) => IntType
      case Add(l,r) if exprsMatchType(l, r, IntType) => IntType
      case Sub(l,r) if exprsMatchType(l, r, IntType) => IntType

      case Greater(l, r)  if exprsMatchType(l, r, IntType) || exprsMatchType(l, r, CharType) => BoolType
      case GreaterE(l, r) if exprsMatchType(l, r, IntType) || exprsMatchType(l, r, CharType) => BoolType
      case Less(l, r)     if exprsMatchType(l, r, IntType) || exprsMatchType(l, r, CharType) => BoolType
      case LessE(l, r)    if exprsMatchType(l, r, IntType) || exprsMatchType(l, r, CharType) => BoolType

      case Eq(l, r)    if getExprType(l) == getExprType(r) => BoolType
      case NotEq(l, r) if getExprType(l) == getExprType(r) => BoolType

      case And(l, r) if exprsMatchType(l, r, BoolType) => BoolType
      case Or(l, r)  if exprsMatchType(l, r, BoolType) => BoolType

      case Not(x) if getExprType(x) == BoolType => BoolType
      case Chr(x) if getExprType(x) == IntType  => CharType
      case Neg(x) if getExprType(x) == IntType  => IntType
      case Ord(x) if getExprType(x) == CharType => IntType
      case Len(ArrayLiter(_))  => IntType
      case _ => ??? //Error
  }

  def validStmtArgs(stmt: Stmt): Boolean = {
    stmt match
      case Read(lValue) => getLValueType(lValue) == IntType || getLValueType(lValue) == CharType
      case Free(expr) => getExprType(expr) match
        case ArrayType(_, _) => true
        case PairType(_, _) => true
        case _ => false
      case Exit(expr) => getExprType(expr) == IntType
      case Assgn(t, _, rValue) => getRValueType(rValue) == t
      case ReAssgn(lValue, rValue) => getLValueType(lValue) == getRValueType(rValue)
      case WhileDo(_, stmts) => stmts.forall(validStmtArgs(_))
      case IfElse(_, thenStmts, elseStmts) => thenStmts.forall(validStmtArgs(_)) & elseStmts.forall(validStmtArgs(_))
      case Scope(stmts) => stmts.forall(validStmtArgs(_))
      case Return(expr) => ??? // Need to check function
      case Print(expr) => true // Might just be true?
      case Println(expr) => true // // Might just be true?
      case Skip => true
  }

  def newPairHandle(fst: Expr, snd: Expr) = {
        val fstType = getExprType(fst) match {
          case PairType(_, _) => Pair
          case otherType => otherType
        }
        val sndType = getExprType(snd) match {
          case PairType(_, _) => Pair
          case otherType => otherType
        }
        PairType(fstType.asInstanceOf[PairElemType], sndType.asInstanceOf[PairElemType])
      }

  def pairElemHandle(lValue: LValue): PairType = {
    lValue match
      case ident: Ident => getExprType(ident) match
        case PairType(Pair, Pair) => ??? // Check otherside
        case PairType(fstType, sndType) => PairType(fstType,sndType)
        case _ => ??? // Error?
      case ArrayElem(arrayName, index) => ??? // Error?
      case wacc.Fst(_) => ??? // Check otherside
      case wacc.Snd(_) => ??? // Check otherside
  }
}


