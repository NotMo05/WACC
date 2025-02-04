
package wacc
import scala.collection.mutable
// def semParse(prog: Prog) = {
//   validSemFuncs(prog.funcs) && validSemStmts(prog.main)


// We need the AST from syntax analysis to include position information (as a
// secondary parameter list to each of the case classes)
object semantic {
  val semErrors = List.newBuilder[String]
  
  enum VarScope:
    case VAR_DECLARED, VAR_UNDECLARED

  def analyse(prog: Prog): List[String] = {
    prog.funcs.foreach(validFunction)
    prog.main.foreach(validStmtArgs(_))
    semErrors.result()
  }

  def validFunction(func: Func) = {
    func.stmts.foreach(validStmtArgs(_, Some(func.t)))
  }

  val symbolTable: mutable.Map[String, Type] = mutable.Map()

  def getRValueType(rValue: Any): Option[Type] = {
    rValue match
      case expr: Expr => getExprType(expr) match
        case None => semErrors += "error: undeclared variable"; None
        case x => x
      
      // need to do Args list and function check?
      case Call(ident, _) => getExprType(ident) match
        case None => semErrors += "error: undeclared variable"; None
        case x => x
      case ArrayLiter(elems) => arrayLiterHandle(elems)
      case NewPair(fst, snd) => newPairHandle(fst, snd)
      case Fst(lValue) => ??? //pairElemHandle(lValue).t1.asInstanceOf[Type]
      case Snd(lValue) => ??? //pairElemHandle(lValue).t2.asInstanceOf[Type]
  }
  
  def getLValueType(lValue: Any): Option[Type] = {
    lValue match
      case ident: Ident => getExprType(ident)
      case ArrayElem(arrayName, index) =>  ??? // This changes based on length of index
      case Fst(lValue) => ??? //pairElemHandle(lValue).t1.asInstanceOf[Type]
      case Snd(lValue) => ??? //pairElemHandle(lValue).t2.asInstanceOf[Type]
  }

  def getExprType(expr: Any): Option[Type] = {
    expr match 
      case op: Operator => getOperType(op)
      case IntLiteral(_) => Some(IntType)
      case BoolLiteral(_) => Some(BoolType)
      case StringLiteral(_) => Some(StringType)
      case CharLiteral(_) => Some(CharType)
      case ArrayElem(arrayName, _) => getExprType(arrayName)
      // need a validIdent function here REQUIRES SCOPING/SYMBOL TABLE/RENAMING/QUALIFICATION
      // This will either provide a some type or None (undeclared var) 
      case Ident(name) => ??? // symbolTable.getOrElse(name, throw new Exception(s"Undeclared identifier: $name"))
      case NullLiteral => ??? // This should be PairType(AnyType, AnyType)
  }


  private def exprsMatchType(expr1: Expr, expr2: Expr, t: Type): Option[Boolean] = {
    val expr1Type = getExprType(expr1)
    val expr2Type = getExprType(expr2)
    expr1Type match
      case Some(x) => expr2Type match
        case Some(y) => Some(x == t & y == t)
        case _ => semErrors += "error: undeclared variable"; None
      case _ => semErrors += "error: undeclared variable"; None  
  }

  def getOperType(op: Any): Option[BaseType] = {
    op match 
      case Mul(l,r) => exprsMatchType(l, r, IntType) match
        case Some(true) => Some(IntType)
        case Some(_) => semErrors += "error: incompatible types for operator"; None
        case _ => semErrors += "error: undeclared variable"; None
       
      case Div(l,r) => exprsMatchType(l, r, IntType) match
        case Some(true) => Some(IntType)
        case Some(_) => semErrors += "error: incompatible types for operator"; None
        case _ => semErrors += "error: undeclared variable"; None

      case Mod(l,r) => exprsMatchType(l, r, IntType) match
        case Some(true) => Some(IntType)
        case Some(_) => semErrors += "error: incompatible types for operator"; None
        case _ => semErrors += "error: undeclared variable"; None

      case Add(l,r) => exprsMatchType(l, r, IntType) match
        case Some(true) => Some(IntType)
        case Some(_) => semErrors += "error: incompatible types for operator"; None
        case _ => semErrors += "error: undeclared variable"; None

      case Sub(l,r) => exprsMatchType(l, r, IntType) match
        case Some(true) => Some(IntType)
        case Some(_) => semErrors += "error: incompatible types for operator"; None
        case _ => semErrors += "error: undeclared variable"; None
        

      // NEED TO DO THE SAME FOR THESE SOMEHOW
      case Greater(l, r)  if exprsMatchType(l, r, IntType) == Some(true) || exprsMatchType(l, r, CharType) == Some(true) => Some(BoolType)
      case GreaterE(l, r) if exprsMatchType(l, r, IntType) == Some(true) || exprsMatchType(l, r, CharType) == Some(true) => Some(BoolType)
      case Less(l, r)     if exprsMatchType(l, r, IntType) == Some(true) || exprsMatchType(l, r, CharType) == Some(true) => Some(BoolType)
      case LessE(l, r)    if exprsMatchType(l, r, IntType) == Some(true) || exprsMatchType(l, r, CharType) == Some(true) => Some(BoolType)

      case Eq(l, r)   => {
        val lType = getExprType(l)
        val rType = getExprType(r)
        lType match
          case Some(x) => rType match
            case Some(y) if (x == y) => Some(BoolType) 
            case Some(_) => semErrors += "error: incompatible types for operator"; None
            case _ => semErrors += "error: undeclared variable"; None
          case None => semErrors += "error: undeclared variable"; None
      }
      case NotEq(l, r) => {
        val lType = getExprType(l)
        val rType = getExprType(r)
        lType match
          case Some(x) => rType match
            case Some(y) if (x == y) => Some(BoolType) 
            case Some(_) => semErrors += "error: incompatible types for operator"; None
            case _ => semErrors += "error: undeclared variable"; None
          case None => semErrors += "error: undeclared variable"; None
      }

      case And(l, r) => exprsMatchType(l, r, BoolType) match
        case Some(true) => Some(IntType)
        case Some(_) => semErrors += "error: incompatible types for operator"; None
        case _ => semErrors += "error: undeclared variable"; None
      
      case Or(l, r) => exprsMatchType(l, r, BoolType) match
        case Some(true) => Some(IntType)
        case Some(_) => semErrors += "error: incompatible types for operator"; None
        case _ => semErrors += "error: undeclared variable"; None

      case Not(x) => getExprType(x) match
        case Some(BoolType) => Some(BoolType)
        case Some(_) => semErrors += "error: incompatible type for operator"; None
        case _ => semErrors += "error: undeclared variable"; None

      case Chr(x) => getExprType(x) match
        case Some(IntType) => Some(CharType)
        case Some(_) => semErrors += "error: incompatible type for operator"; None
        case _ => semErrors += "error: undeclared variable"; None

      case Neg(x) => getExprType(x) match
        case Some(IntType) => Some(IntType)
        case Some(_) => semErrors += "error: incompatible type for operator"; None
        case _ => semErrors += "error: undeclared variable"; None

      case Ord(x) => getExprType(x) match
        case Some(CharType) => Some(IntType)
        case Some(_) => semErrors += "error: incompatible type for operator"; None
        case _ => semErrors += "error: undeclared variable"; None

      case Len(x)  => getExprType(x) match
        case Some(ArrayLiter(_)) => Some(IntType)
        case Some(_) => semErrors += "error: incompatible type for operator"; None
        case _ => semErrors += "error: undeclared variable"; None
      case _ => None  
  }

  // There will be duplicate error messages being created at the moment.
  // Errors for rtypes for array literals (and perhaps pairs) will be tricky
  // because of needing to collect all the different types for the errors where
  // the array liter contains different types (at least that's what the 
  // reference compiler appears to do) and in general we need a way to get the
  // line itself printed out in the errors.
  def validStmtArgs(stmt: Any, funcType: Option[Type] = None): Unit = {
    stmt match
      case Read(lValue) => getLValueType(lValue) match
        case Some(IntType) => ()
        case Some(CharType) => () 
        case _ => semErrors += "error: `read` must be followed by an `int` or `char`"
      
      case Free(expr) => getExprType(expr) match
        case Some(ArrayType(_, _)) => ()
        case Some(PairType(_, _)) => ()
        case _ => semErrors += "error: `free` can only be used on `arrays` or `pairs`"
      
      case Exit(expr) => getExprType(expr) match
        case Some(IntType) => ()
        case _ => semErrors += "error: `exit` statement must be provided with an exit code of type `int`"
      // rValue needs to be compatible with t but does it need to BE t? Can it be a subtype of t? Do subtypes of t even exist? 
      // NEED SCOPING/SYMBOL TABLE/RENAMING/QUALIFICATION FOR THIS TO CHECK MULTIPLE ASSIGNMENTS
      case Assgn(t, _, rValue) => getRValueType(rValue) match
        case Some(x) => if (t != x) then semErrors += "error: incompatible types in assignment"
        case _ => semErrors += "error: undeclared variable"
      // rValue needs to be compatible with lValue but does it need to BE lValue? Can it be a subtype of lValue? Do subtypes of lValue even exist?
      // should consider adding string weakening char[] thingy
      case ReAssgn(lValue, rValue) => {
        val lType = getLValueType(lValue)
        val rType = getRValueType(rValue)
        lType match
          case Some(x) => rType match
            case Some(y) => if (x != y) then semErrors += "error: incompatible types in reassignment"
            case _ => semErrors += "error: undeclared variable"
          case _ => semErrors += "error: undeclared variable" 
      }
      case WhileDo(cond, stmts) => {
        getExprType(cond) match
          case Some(BoolType) => ()
          case None => "error: undeclared variable"
          case _ => semErrors += "error: `while` constructs must have condition of type `bool`"
        stmts.foreach(validStmtArgs(_))
      }
      case IfElse(cond, thenStmts, elseStmts) => {
        getExprType(cond) match
          case Some(BoolType) => ()
          case None => "error: undeclared variable"
          case _ => semErrors += "error: `if` constructs must have condition of type `bool`"
        thenStmts.foreach(validStmtArgs(_))
        elseStmts.foreach(validStmtArgs(_))
      }
      case Scope(stmts) => stmts.foreach(validStmtArgs(_))
      
      case Return(expr) => funcType match
        case Some(x) => getExprType(expr) match
          case Some(y) if x == y => ()
          case None => semErrors += "error: undeclared variable"
          case _ => semErrors += "error: type of given `return` expression is incompatible with enclosing function `return` type"
        case _ => semErrors += "error: `return` cannot be called outside function/in main body of program"
      
      case Print(expr) => getExprType(expr) match
        case None => semErrors += "error: undeclared variable"
      
      case Println(expr) => getExprType(expr) match
        case None => semErrors += "error: undeclared variable" 
      
      case Skip => ???
  }

  def validRead(lValue: Any) = {
    val lValueType = getLValueType(lValue)
    lValueType match
      case Some(IntType) => ()
      case Some(CharType) => () 
      case _ => semErrors += "error: `read` must be followed by an `int` or `char`" 
  }

  def newPairHandle(fst: Expr, snd: Expr): Option[PairType] = {
    val fstType = getExprType(fst) match 
      case Some(x) => x match
        case PairType(_, _) => Pair
        case otherType => otherType
      case None => None

    val sndType = getExprType(snd) match 
      case Some(y) => y match
        case PairType(_, _) => Pair
        case otherType => otherType
      case None => None
    
    if (fstType == None) {
      semErrors += "error: undeclared variable"
      None
    }
    else if (sndType == None) {
      semErrors += "error: undeclared variable"
      None
    }
    else {
      Some(PairType(fstType.asInstanceOf[PairElemType], sndType.asInstanceOf[PairElemType]))
    }
  }

  def pairElemHandle(lValue: LValue): PairType = {
    lValue match
      case ident: Ident => getExprType(ident) match
        case Some(PairType(Pair, Pair)) => ??? // Check otherside
        case Some(PairType(fstType, sndType)) => PairType(fstType,sndType)
        case _ => ??? // Error?
      case ArrayElem(arrayName, index) => ??? // Error?
      case wacc.Fst(_) => ??? // Check otherside
      case wacc.Snd(_) => ??? // Check otherside
  }

  def arrayLiterHandle(elems: List[Expr]): Option[ArrayType] = {
    // get the distinct list of the types of the expressions in the list
    // Likely need to be doing this in a for loop like below so that we have 
    // access to each Expr's position info
    val arrayTypesBuilder = List.newBuilder[Option[Type]]
    
    for (expr <- elems) {
      val exprType = getExprType(expr)
      if (exprType == None) then semErrors += "error: Undeclared variable in array"
      arrayTypesBuilder += exprType
    }
    val potentialArrayTypes: List[Option[Type]] = arrayTypesBuilder.result()
    
    // Means that array liter had undeclared variable(s)
    if (potentialArrayTypes.contains(None)) {
      None
    }
    else {
      // get rid of duplicate Option[Type]s
      val distinctPotentialArrayTypes: List[Option[Type]] = potentialArrayTypes.distinct
      // Unwrap any remaining Some(Type)s
      val distinctArrayTypes: List[Type] = distinctPotentialArrayTypes.flatten
      // Means that array liter was empty
      if (distinctArrayTypes.isEmpty) {
        Some(ArrayType(AnyType, 1))
      }
      // Means that array liter had element(s) of 1 type
      else if (distinctArrayTypes.size == 1) {
        val arrayType = distinctArrayTypes.head
        arrayType match
          case ArrayType(t, d) => Some(ArrayType(t, d + 1))
          case baseOrPairType => Some(ArrayType(baseOrPairType, 1))
      }
      // Means array liter had elements of different types
      else {
        semErrors += "error: literal contains mix of different types"
        None
      }
    } 
  }
}


  