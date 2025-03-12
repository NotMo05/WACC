package wacc.front_end

import java.nio.file.{Files, Paths}

object semantic {
  val semErrors = List.newBuilder[String]
  var functionMap: Map[String, Func] = Map()

  def analyse(prog: Prog): List[String] = {
    semErrors.clear()
    functionMap = prog.funcs.map(f => f.identifier.identifier -> f).toMap
    prog.funcs.foreach(validFunction)
    prog.main.foreach(stmt => validStmtArgs(stmt))
    semErrors.result()
  }

  // Validates a function by checking the types of its statements.
  def validFunction(func: Func) = {
    func.stmts.foreach(validStmtArgs(_, Some(func.t)))
  }

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

  def getRValueType(rValue: Any): Option[Type] = {
    rValue match
      case expr: Expr => getExprType(expr) match
        case None => None
        case t => t

      // Check right num and types of arguments in function call
      case Call(ident, args) => 
        println(s"DEBUG: Processing function call with ident: $ident (${ident.getClass.getName})")
        
        def checkFunction(funcName: String): Option[Type] = {
          if (funcName.isEmpty) {
            semErrors += s"Empty function name in function call"
            None
          } else if (functionMap.contains(funcName)) {
            val func = functionMap(funcName)
            // Check argument count separately from argument types
            if (func.params.size != args.size) {
              semErrors += s"Function call to '$funcName' has incorrect number of arguments: expected ${func.params.size}, got ${args.size}"
              None
            } else if (!func.params.zip(args).forall((param, arg) => {
              getExprType(arg) match {
                case Some(argType) => param.t weakensTo argType
                case None => false
              }
            })) {
              // Print more detailed type mismatch info if needed
              val argTypeInfo = func.params.zip(args).zipWithIndex.map { case ((param, arg), idx) =>
                val argType = getExprType(arg).map(typeToString).getOrElse("unknown")
                val paramType = typeToString(param.t)
                s"arg ${idx+1}: expected $paramType, got $argType"
              }.mkString(", ")
              semErrors += s"Function call to '$funcName' has arguments of incorrect type: $argTypeInfo"
              None
            } else {
              Some(func.t)
            }
          } else {
            semErrors += s"Undefined function '$funcName'"
            None
          }
        }
        
        ident match
          case qn: QualifiedFunc if qn.t == Undefined => 
            val funcName = qn.identifier
            println(s"DEBUG: QualifiedFunc name: '$funcName'")
            checkFunction(funcName)
            
          case regularIdent: Ident =>
            val funcName = regularIdent.identifier
            println(s"DEBUG: Regular ident name: '$funcName'") 
            checkFunction(funcName)

      case ArrayLiter(elems) => arrayLiterHandle(elems)
      case NewPair(fst, snd) => newPairHandle(fst, snd)
      case Fst(lValue) => pairElemHandle(lValue) match
        case None => semErrors += "Expected a valid type here in pair first"; None
        case Some(PairType(Pair, _)) => Some(PairType(AnyType, AnyType))
        case Some(PairType(t1, _)) => Some(t1.asInstanceOf[Type])
      case Snd(lValue) => pairElemHandle(lValue) match
        case None => semErrors += "Expected a valid type here in pair second"; None
        case Some(PairType(_, Pair)) => Some(PairType(AnyType, AnyType))
        case Some(PairType(_, t2)) => Some(t2.asInstanceOf[Type])
  }

  def getLValueType(lValue: Any): Option[Type] = {
    lValue match
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
  }

  def getExprType(expr: Expr): Option[Type] = {
    expr match
      case qn: QualifiedName => qn.t match
        case Undefined => None 
        case t => Some(t)
      case ident: Ident => 
        println(s"DEBUG: Processing Ident: ${ident.identifier}")
        // Rest of your code...
        None
      case op: Operator => getOperType(op)
      case IntLiteral(_) => Some(IntType)
      case BoolLiteral(_) => Some(BoolType)
      case StringLiteral(_) => Some(StringType)
      case CharLiteral(_) => Some(CharType)
      case ArrayElem(arrayName, index) => arrayElemHandle(arrayName, index)
      case NullLiteral => Some(PairType(AnyType, AnyType))
  }

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
        case Some(y) => semErrors += s"TypeK error: unexpected ${typeToString(y)} expected ${typeToString(x)}"; Some(false)
        case None => None
      case None => None
  }

  def getOperType(op: Operator): Option[BaseType] = {

    op match
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
  }

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
      case Assgn(t, _, rValue) =>
        getRValueType(rValue) match
        case Some(x) =>
          if !(t weakensTo x) then
            semErrors += s"Type error: unexpected ${typeToString(x)} expected ${typeToString(t)} in assignment"
        case None => ()
      case ReAssgn(lValue, rValue) => {
        val lType = getLValueType(lValue)
        val rType = getRValueType(rValue)
        lType match
          case Some(x) => rType match
            case Some(y) =>
              if !(x weakensTo y) then
                semErrors += s"Type error: unexpected ${typeToString(y)} expected ${typeToString(x)} in reassign"
            case None => ()
          case None => ()
      }
      case WhileDo(cond, stmts) => {
        getExprType(cond) match
          case Some(BoolType) => ()
          case Some(_) => semErrors += "error: `while` constructs must have condition of type `bool`"
          case None => ()
        stmts.foreach(validStmtArgs(_, funcType))
      }
      case IfElse(cond, thenStmts, elseStmts) => {
        getExprType(cond) match
          case Some(BoolType) => ()
          case Some(_) => semErrors += "error: `if` constructs must have condition of type `bool`"
          case None => ()
        thenStmts.foreach(validStmtArgs(_, funcType))
        elseStmts.foreach(validStmtArgs(_, funcType))
      }
      case Scope(stmts) => stmts.foreach(validStmtArgs(_))

      case Return(expr) => funcType match
        case Some(x) => getExprType(expr) match
          case Some(y) if x weakensTo y => ()
          case Some(z) => semErrors +=
            s"error: ${typeToString(z)} `return` is incompatible with enclosing ${typeToString(x)} function"
          case None => ()
        case None => semErrors += "error: `return` cannot be called outside function/in main body of program"

      case Print(expr) => getExprType(expr)

      case Println(expr) => getExprType(expr)

      case Import(filePath) => {
        val path = filePath.string
        if (path.nonEmpty && Files.exists(Paths.get(path))) {
          processImport(path)
        } else {
          semErrors += s"error: import path '$path' does not exist"
        }
      }

      case Skip => ()
  }

  def processImport(path: String): Unit = {
    try {
      // Read the file content
      val content = new String(Files.readAllBytes(Paths.get(path)))
      
      // Parse the file to get an AST
      val parseResult = parser.parse(content)
      
      // Extract function definitions and add to functionMap
      parseResult match {
        case parsley.Success(program) => program match {
          case Prog(funcs, _) =>
            println(s"DEBUG: Found ${funcs.size} functions in imported file")
            funcs.foreach { func =>
              val funcName = func.identifier.identifier
              if (funcName.nonEmpty) {
                println(s"DEBUG: Importing function '$funcName' with ${func.params.size} parameters")
                functionMap += (funcName -> func)
                
                // IMPORTANT: Also update funcTypes in renaming module to keep them in sync
                renaming.funcTypes(funcName) = new renaming.QualifiedFunc(
                  func.t, 
                  funcName, 
                  func.params.size,  // Set correct parameter count
                  func.params.map(_.t)  // Set correct parameter types
                )
                
                println(s"DEBUG: imported $funcName with body: ${func}")
              } else {
                semErrors += s"Warning: Skipped importing function with empty name"
                println(s"DEBUG: Found function with empty name: $func")
              }
            }
        }
        case parsley.Failure(msg) => 
          semErrors += s"error: imported file '$path' is not a valid WACC program: $msg"
          println(s"DEBUG: Failed to parse imported file: $msg")
      }
    } catch {
      case e: Exception => 
        semErrors += s"error processing import '$path': ${e.getMessage}"
        println(s"DEBUG: Exception while importing: ${e.getMessage}")
        e.printStackTrace()
    }
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
          case Some(pairType: PairType)=> Some(pairType)
          case _ => semErrors += s"Expected pair type here, this var is ${getExprType(ident)}"; None

      case ArrayElem(arrayName, index) =>
        arrayElemHandle(arrayName, index) match
          case Some(pairType: PairType)=> Some(pairType)
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