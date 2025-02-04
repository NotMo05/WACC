package wacc
// def semParse(prog: Prog) = {
//   validSemFuncs(prog.funcs) && validSemStmts(prog.main)


object semantic {

  def analyse(prog: Prog) = {
    prog.main.forall(validStmtArgs(_))
  }

  def getRValueType(rValue: RValue): Type = {
    rValue match
      case expr: Expr => getExprType(expr)
      case Call(ident, _) => getExprType(ident)
      case ArrayLiter(elems) => arrayLiterHandle(elems)
      case NewPair(fst, snd) => newPairHandle(fst, snd)
      case Fst(lValue) => pairElemHandle(lValue).t1.asInstanceOf[Type]
      case Snd(lValue) => pairElemHandle(lValue).t2.asInstanceOf[Type]
  }
  
  def getLValueType(lValue: LValue): Type = {
    lValue match
      case ident: Ident => getExprType(ident)
      case ArrayElem(arrayName, index) => arrayElemHandle(arrayName, index.size)
      case Fst(lValue) => pairElemHandle(lValue).t1 match
        case t: Type => t
        case Pair => ??? //Check otherside, as long as its a pairType, trust it even if its wrong
      case Snd(lValue) => pairElemHandle(lValue).t2 match
        case t: Type => t
        case Pair => ??? //Check otherside, as long as its a pairType, trust it even if its wrong
  }

  def getExprType(expr: Expr): Type = {
    expr match 
      case qn: QualifiedName => qn.t
      case op: Operator => validOpArgs(op)
      case IntLiteral(_) => IntType
      case BoolLiteral(_) => BoolType
      case StringLiteral(_) => StringType
      case CharLiteral(_) => CharType
      case ArrayElem(arrayName, index) => arrayElemHandle(arrayName, index.size)
      case NullLiteral => ??? // Not an Error, This should be PairType(AnyType, AnyType)
      case wacc.Ident(_) => ??? // Error, should've been turned into QualifiedName, shouldn't ever happen invalid code or not

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
      case Len(x) if getExprType(x).isInstanceOf[ArrayType] => IntType
      case _ => ??? //Error?  
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
      case WhileDo(cond, stmts) => getExprType(cond) == BoolType & stmts.forall(validStmtArgs(_))
      case IfElse(cond, thenStmts, elseStmts) => 
        getExprType(cond) == BoolType
        & thenStmts.forall(validStmtArgs(_)) 
        & elseStmts.forall(validStmtArgs(_))
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
      case ident: Ident => 
        getExprType(ident) match
          case pairType: PairType => pairType
          case _ => ??? // Error?

      case ArrayElem(arrayName, index) => 
        arrayElemHandle(arrayName, index.size) match
          case pairType: PairType => pairType
          case _ => ??? // Error?
      case wacc.Fst(lValue) => pairElemHandle(lValue) match
        case PairType(Pair, _) => ??? /// AnyType?
        case _ => ??? // Error, called fst twice at this point so should atleast be a pair with a pair inside
        
      
      
      case wacc.Snd(lValue) => pairElemHandle(lValue) match
        case PairType(_, Pair) => ??? /// AnyType?
        case _ => ??? // Error, called fst twice at this point so should atleast be a pair with a pair inside
  }

  def arrayLiterHandle(elems: List[Expr]) = {
    val arrayTypes = elems.map(getExprType(_)).distinct
    if arrayTypes.size == 1 then arrayTypes.head match
      case ArrayType(t, d) => ArrayType(t, d+1)
      case litOrPairType => ArrayType(litOrPairType, 1)   
    else ??? // Error, Array has multiple types inside
  }

  def arrayElemHandle(arrayName: Ident, dimensionAccess: Int) = {
    getExprType(arrayName) match
      case ArrayType(t, 1) if dimensionAccess == 1 => t 
      case ArrayType(t, d) if dimensionAccess == d => t
      case ArrayType(t, d) if dimensionAccess < d-1 => ArrayType(t, d-1)
      case ArrayType(t, d) if dimensionAccess > d => ??? // Error, ( doing x[1][1][1][1] on a dimension 1 array for example)
      case _ => ??? // Error, the array mentioned in arrayName should have type ArrayType Error
  }
}


