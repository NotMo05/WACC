package wacc
// We need the AST from syntax analysis to include position information (as a
// secondary parameter list to each of the case classes)
object semantic {
  val semErrors = List.newBuilder[String]

  def analyse(prog: Prog): List[String] = {
    semErrors.clear()
    prog.funcs.foreach(validFunction(_))
    prog.main.foreach(validStmtArgs(_))
    semErrors.result()
  }

  def validFunction(func: Func) = {
    func.stmts.foreach(validStmtArgs(_, Some(func.t)))
  }

  def getRValueType(rValue: Any): Option[Type] = {
    rValue match
      case expr: Expr => getExprType(expr) match
        case None => None
        case t => t

      // need to do Args list and function check?
      case Call(ident, args) => ident match
        case qn: QualifiedFunc if qn.paramNum == args.size && qn.paramTypes == args.map(getExprType(_)) => Some(qn.t)
        case _ => None
        // Check right num and type of arguments

// //  wrong number of arguments provided to function x
// //   unexpected 0 arguments
// //   expected 1 arguments

      case ArrayLiter(elems) => arrayLiterHandle(elems)
      case NewPair(fst, snd) => newPairHandle(fst, snd)
      case Fst(lValue) => ??? //pairElemHandle(lValue).t1.asInstanceOf[Type]
      case Snd(lValue) => ??? //pairElemHandle(lValue).t2.asInstanceOf[Type]
  }

  def getLValueType(lValue: Any): Option[Type] = {
    lValue match
      case ident: Ident => getExprType(ident)
      case ArrayElem(arrayName, index) => arrayElemHandle(arrayName, index.size)
      case Fst(lValue) => pairElemHandle(lValue) match
        case None => ???
        case Some(value) => value.t1 match
          case t: Type => Some(t)
          case Pair => ??? //Check otherside, as long as its a pairType, trust it even if its wrong

      case Snd(lValue) => pairElemHandle(lValue) match
        case None => ???
        case Some(value) => value.t2 match
          case t: Type => Some(t)
          case Pair => ??? //Check otherside, as long as its a pairType, trust it even if its wrong
  }

  def getExprType(expr: Any): Option[Type] = {
    expr match
      case qn: QualifiedName => qn.t match
        case x => Some(x) // NEED TO MAKE A UNDECLARED TYPE TO MATCH HERE
        case _ => ???
      case op: Operator => getOperType(op)
      case IntLiteral(_) => Some(IntType)
      case BoolLiteral(_) => Some(BoolType)
      case StringLiteral(_) => Some(StringType)
      case CharLiteral(_) => Some(CharType)
      case ArrayElem(arrayName, index) => arrayElemHandle(arrayName, index.size)
      case NullLiteral => Some(PairType(AnyType, AnyType))
  }


  private def exprsMatchType(expr1: Expr, expr2: Expr, t: Type): Option[Boolean] = {
    val expr1Type = getExprType(expr1)
    val expr2Type = getExprType(expr2)
    expr1Type match
      case Some(x) => expr2Type match
        case Some(y) => Some(x == t & y == t)
        case _ => None
      case _ => None
  }

  def getOperType(op: Any): Option[BaseType] = {
    op match
      case Mul(l,r) => exprsMatchType(l, r, IntType) match
        case Some(true) => Some(IntType)
        case Some(_) => semErrors += "error: incompatible types for operator"; None
        case _ => None

      case Div(l,r) => exprsMatchType(l, r, IntType) match
        case Some(true) => Some(IntType)
        case Some(_) => semErrors += "error: incompatible types for operator"; None
        case _ => None

      case Mod(l,r) => exprsMatchType(l, r, IntType) match
        case Some(true) => Some(IntType)
        case Some(_) => semErrors += "error: incompatible types for operator"; None
        case _ => None

      case Add(l,r) => exprsMatchType(l, r, IntType) match
        case Some(true) => Some(IntType)
        case Some(_) => semErrors += "error: incompatible types for operator"; None
        case _ => None

      case Sub(l,r) => exprsMatchType(l, r, IntType) match
        case Some(true) => Some(IntType)
        case Some(_) => semErrors += "error: incompatible types for operator"; None
        case _ => None


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
            case _ => None
          case None => None
      }
      case NotEq(l, r) => {
        val lType = getExprType(l)
        val rType = getExprType(r)
        lType match
          case Some(x) => rType match
            case Some(y) if (x == y) => Some(BoolType)
            case Some(_) => semErrors += "error: incompatible types for operator"; None
            case _ => None
          case None => None
      }

      case And(l, r) => exprsMatchType(l, r, BoolType) match
        case Some(true) => Some(BoolType)
        case Some(_) => semErrors += "error: incompatible types for operator"; None
        case _ => None

      case Or(l, r) => exprsMatchType(l, r, BoolType) match
        case Some(true) => Some(BoolType)
        case Some(_) => semErrors += "error: incompatible types for operator"; None
        case _ => None

      case Not(x) => getExprType(x) match
        case Some(BoolType) => Some(BoolType)
        case Some(_) => semErrors += "error: incompatible type for operator"; None
        case _ => None

      case Chr(x) => getExprType(x) match
        case Some(IntType) => Some(CharType)
        case Some(_) => semErrors += "error: incompatible type for operator"; None
        case _ => None

      case Neg(x) => getExprType(x) match
        case Some(IntType) => Some(IntType)
        case Some(_) => semErrors += "error: incompatible type for operator"; None
        case _ => None

      case Ord(x) => getExprType(x) match
        case Some(CharType) => Some(IntType)
        case Some(_) => semErrors += "error: incompatible type for operator"; None
        case _ => None

      case Len(x)  => getExprType(x) match
        case Some(ArrayType(_,_)) => Some(IntType)
        case Some(_) => semErrors += "error: incompatible type for operator"; None
        case _ => None
      case _ => None
  }

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
        case Some(x) => {
          if !(t weakensTo x) then semErrors += "error: incompatible types in assignment"}
        case _ => ()
      // rValue needs to be compatible with lValue but does it need to BE lValue? Can it be a subtype of lValue? Do subtypes of lValue even exist?
      // should consider adding string weakening char[] thingy
      case ReAssgn(lValue, rValue) => {
        val lType = getLValueType(lValue)
        val rType = getRValueType(rValue)
        lType match
          case Some(x) => rType match
            case Some(y) => if (x != y) then semErrors += "error: incompatible types in reassignment"
            case _ => ()
          case _ => ()
      }
      case WhileDo(cond, stmts) => {
        getExprType(cond) match
          case Some(BoolType) => ()
          case None => ()
          case _ => semErrors += "error: `while` constructs must have condition of type `bool`"
        stmts.foreach(validStmtArgs(_, funcType))
      }
      case IfElse(cond, thenStmts, elseStmts) => {
        getExprType(cond) match
          case Some(BoolType) => ()
          case None => ()
          case _ => semErrors += "error: `if` constructs must have condition of type `bool`"
        thenStmts.foreach(validStmtArgs(_, funcType))
        elseStmts.foreach(validStmtArgs(_, funcType))
      }
      case Scope(stmts) => stmts.foreach(validStmtArgs(_))

      case Return(expr) => funcType match
        case Some(x) => getExprType(expr) match
          case Some(y) if x == y => ()
          case None => ()
          case _ => semErrors += "error: type of given `return` expression is incompatible with enclosing function `return` type"
        case _ => semErrors += "error: `return` cannot be called outside function/in main body of program"

      case Print(expr) => getExprType(expr) match
        case None => ()
        case _ => ()

      case Println(expr) => getExprType(expr) match
        case None => ()
        case _ => ()

      case Skip => ()
  }
//   2.3.4 Sequentialisation and No-ops
// The ‘skip’ statement has no effect on the program when executed. Given ‘𝑆1 ; 𝑆2’, first, the statement
// 𝑆1 is executed, and then the statement 𝑆2 is executed, observing any changes that 𝑆1 may have made
// during execution. Note that ‘skip ; 𝑆’ and ‘𝑆 ; skip’ MUST both be semantically equivalent to ‘𝑆’.
// The ‘skip’ statement can be used to ignore unused branches of conditional statements, for instance

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
      None
    }
    else if (sndType == None) {
      None
    }
    else {
      Some(PairType(fstType.asInstanceOf[PairElemType], sndType.asInstanceOf[PairElemType]))
    }
  }

  def pairElemHandle(lValue: LValue): Option[PairType] = {
    lValue match
      case ident: Ident =>
        getExprType(ident) match
          case Some(pairType: PairType)=> Some(pairType)
          case _ => ??? // Error?

      case ArrayElem(arrayName, index) =>
        arrayElemHandle(arrayName, index.size) match
          case Some(pairType: PairType)=> Some(pairType)
          case _ => ??? // Error?
      case wacc.Fst(lValue) => pairElemHandle(lValue) match
        case Some(PairType(Pair, _)) => ??? /// AnyType?
        case _ => ??? // Error, called fst twice at this point so should atleast be a pair with a pair inside



      case wacc.Snd(lValue) => pairElemHandle(lValue) match
        case Some(PairType(_, Pair)) => ??? /// AnyType?
        case _ => ??? // Error, called fst twice at this point so should atleast be a pair with a pair inside
  }

  def arrayLiterHandle(elems: List[Expr]): Option[ArrayType] = {
    // get the distinct list of the types of the expressions in the list
    // Likely need to be doing this in a for loop like below so that we have
    // access to each Expr's position info

    val potentialArrayTypes: List[Option[Type]] = elems.map(getExprType(_)).toList

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
      print(distinctArrayTypes.size)
      distinctArrayTypes.size match
        case 0 => Some(ArrayType(AnyType, 1))
        case 1 => {
          val arrayType = distinctArrayTypes.head
          arrayType match
            case ArrayType(t, d) => Some(ArrayType(t, d + 1))
            case baseOrPairType => Some(ArrayType(baseOrPairType, 1))
        }
        case 2 => {
          val t1 = distinctArrayTypes(0)
          val t2 = distinctArrayTypes(1)
          (t1, t2) match {
            case (ArrayType(_,d1), ArrayType(_,d2)) => {
              if t1 weakensTo t2 then Some((ArrayType(t1, d1 + 1)))
              else if t2 weakensTo t1 then Some((ArrayType(t2, d1 + 1)))
              else None
            }
            case (_, _) => {
              if t1 weakensTo t2 then Some((ArrayType(t1, 1)))
              else if t2 weakensTo t1 then Some((ArrayType(t2, 1)))
              else None
            }
          }
        }
        case _ => semErrors += "error: literal contains mix of different types"; None
      }
    }

  def arrayElemHandle(arrayName: Ident, dimensionAccess: Int) = {
    getExprType(arrayName) match
      case Some(ArrayType(t, d)) if dimensionAccess == d => Some(t)
      case Some(ArrayType(t, d)) if dimensionAccess < d => Some(ArrayType(t, d-dimensionAccess))
      case _ => {
        semErrors += s"Expected $dimensionAccess-dimensional Array"
        None
      }
  }
}