package wacc.front_end

import javax.lang.model.`type`.ErrorType


object semantic {
  val semErrors = List.newBuilder[String]
  val newStmts = List.newBuilder[Stmt]
  val newFuncs = List.newBuilder[Func]
  var functionMap: Map[String, Func] = Map()

  def analyse(prog: Prog): (Prog, List[String]) = {
    semErrors.clear()
    functionMap = prog.funcs.map(f => f.identifier.identifier -> f).toMap
    val newFuncs = prog.funcs.map(validFunction(_))
    val newStmts = prog.main.map(stmt => validStmtArgs(stmt))
    println(Prog(newFuncs, newStmts).prettyPrint())
    (Prog(newFuncs, newStmts), semErrors.result())
  }

  // Validates a function by checking the types of its statements.
  def validFunction(func: Func): Func = Func(
    func.t,
    func.identifier,
    func.params,
    func.stmts.map(validStmtArgs(_, Some(func.t))))

  private def typeToString(t: Type): String = t match
    case PairType(t1, t2) =>
      (t1,t2) match
        case (Pair, Pair) => "pair(pair, pair)"
        case (x, Pair) => s"pair(${typeToString(x.asInstanceOf[Type])}, pair)"
        case (Pair, x) => s"pair(pair, ${typeToString(x.asInstanceOf[Type])})"
        case (x,y) => s"pair(${typeToString(x.asInstanceOf[Type])}, ${typeToString(y.asInstanceOf[Type])})"
    case ArrayType(t, d) => s"${typeToString(t.asInstanceOf[Type])}${"[]" * d}"
    case IntType => "int"
    case BoolType => "bool"
    case StringType => "string"
    case CharType => "char"
    case AnyType => "any type"
    case Undefined => "undefined"

  def getRValueType(
    rValue: RValue,
    lValuetype: Option[Type] = None
  ): (Option[Type], RValue) =
    rValue match
      case expr: Expr => getExprType(expr) match
        case None => (None, rValue)
        case t => (t, rValue)

      case ArrayLiter(elems) => (arrayLiterHandle(elems), rValue)
      case NewPair(fst, snd) => (newPairHandle(fst, snd), rValue)
      case Fst(lValue) => pairElemHandle(lValue) match
        case None => semErrors += "Expected a valid type here in pair first"; (None, rValue)
        case Some(PairType(Pair, _)) => (Some(PairType(AnyType, AnyType)), rValue)
        case Some(PairType(t1, _)) => (Some(t1.asInstanceOf[Type]), rValue)
      case Snd(lValue) => pairElemHandle(lValue) match
        case None => semErrors += "Expected a valid type here in pair second"; (None, rValue)
        case Some(PairType(_, Pair)) => (Some(PairType(AnyType, AnyType)), rValue)
        case Some(PairType(_, t2)) => (Some(t2.asInstanceOf[Type]), rValue)

      case PossibleCalls(ident: Ident, possibleFuncs, args) =>
        val t = lValuetype.getOrElse(Undefined)
        ident match
          case Ident(identifier) => {
            val argTypes = args.flatMap(getExprType(_))

            // Check if any function has a matching return type
            val matchingReturnType = possibleFuncs.get(args.length) match
              case None => false
              case Some(value) => value.exists(qf => t weakensTo qf.t)

            // Find the first function that matches both return type and argument types
            possibleFuncs.get(args.length) match
              case None => {
                val msg = s"Type error: No function $identifier with ${args.length} argument(s) found"
                semErrors += msg
                (None, ErrorStmt(msg))
              }
              case Some(value) => value.find(qf =>
                (t weakensTo qf.t) &&
                qf.paramTypes.zip(argTypes).forall((a, b) => a weakensTo b)
            ) match {
              case Some(qf) => println("got here\n\n\n\n\n"); (Some(qf.t), Call(qf, args))
              case None =>
                val msg =
                  if (matchingReturnType)
                    s"Type error: No function $identifier with arguments of types matching $args found"
                  else
                    s"Type error: No function $identifier with return type $t found"

                semErrors += msg
                (None, ErrorStmt(msg))
            }
          }
      case Call(_, _) => ???
      case err: ErrorStmt => (None, err)



  def getLValueType(lValue: Any): Option[Type] = lValue match
    case ident: Ident => getExprType(ident)
    case ArrayElem(arrayName, index) => arrayElemHandle(arrayName, index)
    case Fst(lValue) => pairElemHandle(lValue) match
      case None => semErrors += "Expected a valid type here in pair first";None
      case Some(PairType(Pair, _)) => Some(PairType(AnyType, AnyType))
      case Some(PairType(t1, _)) => Some(t1.asInstanceOf[Type])
    case Snd(lValue) => pairElemHandle(lValue) match
      case None => semErrors += "Expected a valid type here in pair second";None
      case Some(PairType(_, Pair)) => Some(PairType(AnyType, AnyType))
      case Some(PairType(_, t2)) => Some(t2.asInstanceOf[Type])

  def getExprType(expr: Expr): Option[Type] = expr match
    case qn: QualifiedName => qn.t match
      case Undefined => None
      case t => Some(t)
    case ident: Ident => None
    case op: Operator => getOperType(op)
    case IntLiteral(_) => Some(IntType)
    case BoolLiteral(_) => Some(BoolType)
    case StringLiteral(_) => Some(StringType)
    case CharLiteral(_) => Some(CharType)
    case ArrayElem(arrayName, index) => arrayElemHandle(arrayName, index)
    case NullLiteral => Some(PairType(AnyType, AnyType))

  private def exprsMatchType(expr1: Expr, expr2: Expr, t: Type): Option[Boolean] = {
    val expr1Type = getExprType(expr1)
    val expr2Type = getExprType(expr2)
    expr1Type match
      case Some(x) if x == t => expr2Type match
        case Some(y) if y == t => Some(true)
        case Some(y) => semErrors += s"Type error: unexpected ${typeToString(y)} expected ${typeToString(t)}"; Some(false)
        case _ => None
      case Some(x) => expr2Type match
        case Some(y) if y == t => semErrors +=
          s"Type error: unexpected ${typeToString(x)} expected ${typeToString(t)}"; Some(false)
        case Some(y) => semErrors +=
          s"Type error: unexpected ${typeToString(x)} and ${typeToString(y)} expected '${typeToString(t)}'s"; Some(false)
        case None => None
      case None => None
  }

  private def exprsMatchOneOfTypes(expr1: Expr, expr2: Expr, t1: Type, t2: Type): Option[Boolean] = {
    val expr1Type = getExprType(expr1)
    val expr2Type = getExprType(expr2)
    expr1Type match
      case Some(x) if (x == t1 || x == t2) => expr2Type match
        case Some(y) if (y == x) => Some(true)
        case Some(y) => semErrors += s"Type error: unexpected ${typeToString(y)} expected ${typeToString(x)}"; Some(false)
        case None => None
      case Some(x) => expr2Type match
        case Some(y) if (y == t1 || y == t2) => semErrors +=
          s"Type error: unexpected ${typeToString(x)} expected ${typeToString(y)}"; Some(false)
        case Some(y) => {
          semErrors +=
          s"Type error: unexpected ${typeToString(x)} and ${typeToString(y)} expected to be both ${typeToString(t1)} or both ${typeToString(t2)}"
          Some(false)
        }
        case None => None
      case None => None
  }

  private def exprsAreOfSameType(expr1: Expr, expr2: Expr): Option[Boolean] = {
    val expr1Type = getExprType(expr1)
    val expr2Type = getExprType(expr2)
    expr1Type match
      case Some(x) => expr2Type match
        case Some(y) if (x weakensTo y) => Some(true)
        case Some(y) => semErrors += s"Type error: unexpected ${typeToString(y)} expected ${typeToString(x)}"; Some(false)
        case None => None
      case None => None
  }

  def getOperType(op: Operator): Option[BaseType] = op match
      case Mul(l,r) => exprsMatchType(l, r, IntType) match
        case Some(true) => Some(IntType)
        case _ => None

      case Div(l,r) => exprsMatchType(l, r, IntType) match
        case Some(true) => Some(IntType)
        case _ => None

      case Mod(l,r) => exprsMatchType(l, r, IntType) match
        case Some(true) => Some(IntType)
        case _ => None

      case Add(l,r) => exprsMatchType(l, r, IntType) match
        case Some(true) => Some(IntType)
        case _ => None

      case Sub(l,r) => exprsMatchType(l, r, IntType) match
        case Some(true) => Some(IntType)
        case _ => None

      case And(l, r) => exprsMatchType(l, r, BoolType) match
        case Some(true) => Some(BoolType)
        case _ => None

      case Or(l, r) => exprsMatchType(l, r, BoolType) match
        case Some(true) => Some(BoolType)
        case _ => None

      case Not(x) => getExprType(x) match
        case Some(BoolType) => Some(BoolType)
        case _ => None

      case Chr(x) => getExprType(x) match
        case Some(IntType) => Some(CharType)
        case _ => None

      case Neg(x) => getExprType(x) match
        case Some(IntType) => Some(IntType)
        case _ => None

      case Ord(x) => getExprType(x) match
        case Some(CharType) => Some(IntType)
        case _ => None

      case Len(x)  => getExprType(x) match
        case Some(ArrayType(_, _)) => Some(IntType)
        case _ => None

      case Greater(l, r) => exprsMatchOneOfTypes(l, r, IntType, CharType) match
        case Some(true) => Some(BoolType)
        case _ => None

      case GreaterE(l, r) => exprsMatchOneOfTypes(l, r, IntType, CharType) match
        case Some(true) => Some(BoolType)
        case _ => None

      case Less(l, r) => exprsMatchOneOfTypes(l, r, IntType, CharType) match
        case Some(true) => Some(BoolType)
        case _ => None

      case LessE(l, r) => exprsMatchOneOfTypes(l, r, IntType, CharType) match
        case Some(true) => Some(BoolType)
        case _ => None

      case Eq(l, r) => exprsAreOfSameType(l, r) match
        case Some(true) => Some(BoolType)
        case _ => None

      case NotEq(l, r) => exprsAreOfSameType(l, r) match
        case Some(true) => Some(BoolType)
        case _ => None


  def validStmtArgs(stmt: Stmt, funcType: Option[Type] = None): Stmt = {
    stmt match
      case Read(lValue) => getLValueType(lValue) match
        case Some(IntType) => Read(lValue)
        case Some(CharType) => Read(lValue)
        case _ => {
          val incorrectRead = "error: `read` must be followed by an `int` or `char`"
          semErrors += incorrectRead
          ErrorStmt(incorrectRead)
        }

      case Free(expr) => getExprType(expr) match
        case Some(ArrayType(_, _)) => Free(expr)
        case Some(PairType(_, _)) => Free(expr)
        case _ =>{
          val incorrectFree = "error: `free` can only be used on `arrays` or `pairs`"
          semErrors += incorrectFree
          ErrorStmt(incorrectFree)
        }

      case Exit(expr) => getExprType(expr) match
        case Some(IntType) => Exit(expr)

        case _ => {
          val exitError = "error: `exit` statement must be provided with an exit code of type `int`"
          semErrors += exitError
          ErrorStmt(exitError)
        }

      case Assgn(t, ident, rValue) =>
        getRValueType(rValue, Some(t)) match
          case (Some(x), newRValue) =>
            if !(t weakensTo x) then {
              val typeError = s"Type error: unexpected ${typeToString(x)} expected ${typeToString(t)} in assignment"
              semErrors += typeError
              ErrorStmt(typeError)
            } else Assgn(t, ident, newRValue)

          case (None, newRValue) => Assgn(t, ident, newRValue)

      case ReAssgn(lValue, rValue) => {
        val lType = getLValueType(lValue)
        val rType = getRValueType(rValue, lType)
        lType match
          case Some(x) => rType match
            case (Some(y), newRValue) =>
              if !(x weakensTo y) then {
                val msg = s"Type error: unexpected ${typeToString(y)} expected ${typeToString(x)} in reassign"
                semErrors += msg
                ErrorStmt(msg)
              } else ReAssgn(lValue, newRValue)
            case (None, newRValue) => ReAssgn(lValue, newRValue)
          case None => ReAssgn(lValue, rType._2)
      }

      case WhileDo(cond, stmts) => {
        getExprType(cond) match
          case Some(BoolType) => ()
          case Some(_) => semErrors += "error: `while` constructs must have condition of type `bool`"
          case None => ()
        WhileDo(cond, stmts.map(validStmtArgs(_, funcType)))
      }

      case IfElse(cond, thenStmts, elseStmts) => {
        getExprType(cond) match
          case Some(BoolType) => ()
          case Some(_) => semErrors += "error: `if` constructs must have condition of type `bool`"
          case None => ()
        IfElse(
          cond,
          thenStmts.map(validStmtArgs(_, funcType)),
          elseStmts.map(validStmtArgs(_, funcType))
        )
      }

      case Scope(stmts) => {
        Scope(stmts.map(validStmtArgs(_))
        )
      }

      case Return(expr) => funcType match
        case Some(x) => getExprType(expr) match
          case Some(y) if x weakensTo y => Return(expr)
          case Some(z) => {
            semErrors +=
            s"error: ${typeToString(z)} `return` is incompatible with enclosing ${typeToString(x)} function"
            Return(expr)
          }
          case None => (Return(expr))
        case None => {
          semErrors += "error: `return` cannot be called outside function/in main body of program"
          Return(expr)
        }

      case Print(expr) => {
        getExprType(expr)
        Print(expr)
      }

      case Println(expr) => {
        getExprType(expr)
        Println(expr)
      }

      case Skip => Skip
  }

  /* Ensures that reading is defined properly with types
  * Builds on errors if incorrect type encountered
  */
  def validRead(lValue: LValue) = {
    val lValueType = getLValueType(lValue)
    lValueType match
      case Some(IntType) => ()
      case Some(CharType) => ()
      case _ => semErrors += "error: `read` must be followed by an `int` or `char`"
  }

  /**
   * Creates a new PairType from two given expressions if both expressions have valid types.
   *
   * @param fst The first expression to be paired.
   * @param snd The second expression to be paired.
   * @return An Option containing the PairType if both expressions have valid types, otherwise None.
   */

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
          case Some(pairType: PairType) => Some(pairType)
          case _ => semErrors += s"Expected pair type here, this var is ${getExprType(ident)}"; None
      case ArrayElem(arrayName, index) =>
        arrayElemHandle(arrayName, index) match
          case Some(pairType: PairType) => Some(pairType)
          case _ => semErrors += s"Expected pair type here, instead found ${arrayElemHandle(arrayName, index)}"; None
      case Fst(lValue) => pairElemHandle(lValue) match
        case Some(PairType(Pair, _)) => Some(PairType(AnyType, AnyType))
        case _ => semErrors += s"Calling fst on a non-pair type, found ${pairElemHandle(lValue)}"; None
      case Snd(lValue) => pairElemHandle(lValue) match
        case Some(PairType(_, Pair)) => Some(PairType(AnyType, AnyType))
        case _ => semErrors += s"Calling snd on a non-pair type, found ${pairElemHandle(lValue)}"; None
  }

  // Need to move error outside this function and potentially return list of different types in array

  /**
   * Handles the creation of an ArrayType from a list of expressions.
   * It checks the types of the expressions in the list and determines the appropriate ArrayType.
   *
   * @param elems The list of expressions to be converted into an ArrayType.
   * @return An Option containing the ArrayType if the expressions have valid types, otherwise None.
   */
  def arrayLiterHandle(elems: List[Expr]): Option[ArrayType] = {
    // Get the list of the types of the expressions in the list
    val potentialArrayTypes: List[Option[Type]] = elems.map(getExprType(_)).toList

    // Means that array literal had undeclared variable(s)
    if (potentialArrayTypes.contains(None)) {
      None
    } else {
      // Get rid of duplicate Option[Type]s
      val distinctArrayTypes: List[Type] = potentialArrayTypes.distinct.flatten

      // Means that array literal was empty
      distinctArrayTypes.size match
        case 0 => Some(ArrayType(AnyType, 1))
        case 1 =>
          val arrayType = distinctArrayTypes.head
          arrayType match
            case ArrayType(t, d) => Some(ArrayType(t, d + 1))
            case baseOrPairType => Some(ArrayType(baseOrPairType, 1))
        case 2 =>
          val t1 = distinctArrayTypes(0)
          val t2 = distinctArrayTypes(1)
          (t1, t2) match
            case (ArrayType(_, d1), ArrayType(_, d2)) =>
              if t1 weakensTo t2 then Some(ArrayType(t1, d1 + 1))
              else if t2 weakensTo t1 then Some(ArrayType(t2, d1 + 1))
              else None
            case (_, _) =>
              if t1 weakensTo t2 then Some(ArrayType(t1, 1))
              else if t2 weakensTo t1 then Some(ArrayType(t2, 1))
              else None
        case _ =>
          semErrors += "error: literal contains mix of different types"
          None
    }
  }

  /** Handling for referencing an elem in a handle
   * @param arrayName name of the array
   * @param index position of the element in the array
   * @return The type of the element
   */
  def arrayElemHandle(arrayName: Ident, index: List[Expr]): Option[Type] = {
    val indexTypes = index.map(getExprType(_)).distinct.flatten
    if indexTypes.size != 1 then {
      val errorIndexTypes = indexTypes.filter(_ != IntType)
      errorIndexTypes.foreach(t => semErrors += s"Type error: unexpected ${t} expected int")
      return None
    }
    if !indexTypes.contains(IntType) then {
      semErrors += s"Type error: unexpected ${indexTypes.head} expected int"
      return None
    }
    val dimensionAccess = index.size
    getExprType(arrayName) match
      case Some(ArrayType(t, d)) if dimensionAccess == d => Some(t)
      case Some(ArrayType(t, d)) if dimensionAccess < d => Some(ArrayType(t, d-dimensionAccess))
      case _ => {
        semErrors += s"Expected $dimensionAccess-dimensional Array"
        None
      }
  }
}
