package wacc.front_end
import scala.collection.mutable.LinkedHashMap
import scala.collection.mutable
import java.nio.file.Files
import java.nio.file.Paths
import java.nio.file.Path

class QualifiedName(val name: String, val num: Int, val t: Type) extends Ident(name) {

  override def toString() = s"$name$num"
  override def equals(obj: Any): Boolean = obj match {
    case that: QualifiedName =>
      this.name == that.name && this.num == that.num && this.t == that.t
    case _ => false
  }
  override def hashCode(): Int = (name, num, t).##
}

class QualifiedFunc(val t: Type, val funcName: String, val paramNum: Int, val paramTypes: List[Type]) extends Ident(funcName) {
  override def toString() = s"QualifiedFunc(t=$t, funcName=$funcName, paramNum=$paramNum, paramTypes=$paramTypes)"
  override def hashCode() = (t, funcName, paramNum, paramTypes).##
}

val funcTypes: mutable.Map[String, mutable.Map[Int, mutable.ListBuffer[QualifiedFunc]]] = mutable.Map()

val globalNumbering: mutable.Map[String, Int] = mutable.Map()

val scopeErrors = List.newBuilder[String]

def globalNumberingUpdate(name: String): Option[Int] = {
  globalNumbering.updateWith(name) {
    case Some(n) => Some(n + 1)
    case None => Some(0)
  }
}

def rename(prog: ProgWithImports): (Prog, List[String]) = {
  scopeErrors.clear()
  globalNumbering.clear()
  funcTypes.clear()
  funcFirstPass(prog.imports, prog.funcs)
  val newProg = combineImportedFuncs(prog)
  return (
    Prog(newProg.funcs.map(funcHandler(_)),
    scopeHandler(newProg.main, Map.empty[String, QualifiedName])),
    scopeErrors.result()
  )
}

def importHandleForRepl(imp: Import): List[Func] = {
  funcFirstPass(List(imp), List.empty)
  combineImportedFuncs(ProgWithImports(List(imp), List.empty, List.empty)).funcs.map(funcHandler(_))
}

def combineImportedFuncs(prog: ProgWithImports): Prog = {
  val importedFuncs = prog.imports.flatMap(imports => imports match
      case Import(filePath) => processImport(filePath.string)
  )
  Prog(importedFuncs ++ prog.funcs, prog.main)
}

def processImport(path: String): List[Func] = {
  try {
    // Resolve the import path
    val resolvedPath = resolveImportPath(path)

    // If path couldn't be resolved, return empty list
    if (resolvedPath.isEmpty) {
      println(s"DEBUG: Import file not found: $path")
      return List()
    }

    println(s"DEBUG: Importing from resolved path: ${resolvedPath.get}")

    // Read the file content
    val content = new String(Files.readAllBytes(resolvedPath.get))

    // Parse the file to get an AST
    val parseResult = parser.parse(content)

    // Extract function definitions
    parseResult match {
      case parsley.Success(program) => program match {
        case ProgWithImports(_, funcs, _) =>
          println(s"DEBUG: Successfully imported ${funcs.size} functions from $path")
          funcs
      }
      case parsley.Failure(msg) =>
        println(s"DEBUG: Failed to parse import file: $path - $msg")
        List()
    }
  } catch {
    case e: Exception =>
      println(s"DEBUG: Error processing import: $path - ${e.getMessage}")
      e.printStackTrace()
      List()
  }
}

/**
 * Resolves an import path to an actual file path.
 * Handles stdlib imports and relative paths.
 *
 * @param path The import path from the WACC file
 * @return An Optional Path to the file, or None if the file couldn't be found
 */
def resolveImportPath(path: String): Option[Path] = {
  val projectRoot = Paths.get(System.getProperty("user.dir"))
  val stdlibDirectPath = projectRoot.resolve("stdlib").resolve(path)
  val relativePath = projectRoot.resolve(path)
  val absolutePath = Paths.get(path)

  // For stdlib imports (explicit paths starting with "stdlib/")
  if (path.startsWith("stdlib/")) {
    val stdlibPath = projectRoot.resolve(path)
    if (Files.exists(stdlibPath)) {
      return Some(stdlibPath)
    }
  } else if (Files.exists(stdlibDirectPath)) {
    // Try to find the file in the stdlib directory first for any import
    return Some(stdlibDirectPath)
  } else if (Files.exists(relativePath)) {
    // Try as a relative path from the current directory
    return Some(relativePath)
  }

  else if (Files.exists(absolutePath)) {
    // Try as an absolute path
    return Some(absolutePath)
  }

  None
}

// check for duplicates
def funcFirstPass(imports: List[Import], funcs: List[Func]) = {
  val uniqueFuncSet: mutable.Set[(String, List[Type], Type)] = mutable.Set()

  def addFunc(func: Func) = {
    val name = func.identifier.identifier
    val paramTypes = func.params.map(_.t)
    val retType = func.t
    if (uniqueFuncSet.contains((name, paramTypes, retType))) {
      scopeErrors += s"Illegal redefinition of function $name"
    } else {
      funcTypes.getOrElseUpdate(name, mutable.Map())
        .getOrElseUpdate(paramTypes.length, mutable.ListBuffer())
        += (QualifiedFunc(retType, name, paramTypes.length, paramTypes))

      uniqueFuncSet.add((name, paramTypes, retType))
    }
  }

  // Process imported functions
  imports.foreach { imp =>
    processImport(imp.filePath.string).map(addFunc(_))
  }

  // Process local funcs
  funcs.map(addFunc(_))
}

def funcHandler(func: Func): Func = {
  val uniqueNames = mutable.Set[String]()
  def paramHandler(param: Param) = {
    val paramName = param.identifier.identifier
    if uniqueNames.contains(paramName) then {
      scopeErrors += s"Scope error: Illegal redeclaration of variable $paramName"
    }
    uniqueNames.add(paramName)
    globalNumberingUpdate(paramName)
    (paramName, QualifiedName(paramName, globalNumbering(paramName), param.t))
  }

  val params = func.params
  val paramScope = params.reverse
    .map(paramHandler)
    .foldLeft(LinkedHashMap[String, QualifiedName]()) { (acc, pair) =>
      acc.getOrElseUpdate(pair._1, pair._2)
      acc
    }
  val qParam = paramScope.map((_,qn) => Param(qn.t, qn)).toList
  Func(
    func.t,
    QualifiedFunc(
      func.t,
      func.identifier.identifier,
      params.length, params.map(_.t)
    ),
    qParam,
    scopeHandler(func.stmts, paramScope.toMap)
  )
}

def lValueHandler(
  lValue: LValue,
  current: mutable.Map[String, QualifiedName],
  parent: Map[String, QualifiedName]
  ): LValue = {
  lValue match
    case Ident(identifier) => renameIdent(identifier, current, parent)
    case arrayElem: ArrayElem => exprHandler(arrayElem, current, parent).asInstanceOf[LValue]
    case Fst(lValue) => Fst(lValueHandler(lValue, current, parent))
    case Snd(lValue) => Snd(lValueHandler(lValue, current, parent))
}

def rValueHandler(
  rValue: RValue,
  current: mutable.Map[String, QualifiedName],
  parent: Map[String, QualifiedName]
  ): RValue = {
    (rValue: @unchecked) match
      case expr: Expr => exprHandler(expr, current, parent)
      case Call(ident, args) if funcTypes.contains(ident.identifier) => {
          PossibleCalls(
            ident,
            funcTypes(ident.identifier).view.mapValues(_.toList).toMap, // Convert ListBuffer to List
            args.map(exprHandler(_, current, parent))
          )
        }

      case Fst(lValue) => Fst(lValueHandler(lValue, current, parent))
      case Snd(lValue) => Snd(lValueHandler(lValue, current, parent))
      case ArrayLiter(elems) => ArrayLiter(elems.map(exprHandler(_, current, parent)))
      case NewPair(fst, snd) =>
        NewPair(exprHandler(fst, current, parent), exprHandler(snd, current, parent))
      case c: Call => {
        val msg = s"Function ${c.ident.identifier} has not been defined"
        scopeErrors += msg
        ErrorStmt(msg)
      }
    }

def renameStmt(
  stmt: Stmt,
  current: mutable.Map[String, QualifiedName],
  parent: Map[String, QualifiedName]
): Stmt = {
  (stmt: @unchecked) match
    case Skip => Skip
    case Assgn(t, Ident(name), rValue) => renameAssign(t, name, rValue, current, parent)
    case ReAssgn(lValue, rValue) =>
      ReAssgn(lValueHandler(lValue, current, parent), rValueHandler(rValue, current, parent))
    case Read(lValue) => Read(lValueHandler(lValue, current, parent))
    case Free(expr) => Free(exprHandler(expr, current, parent))
    case Return(expr) => Return(exprHandler(expr, current, parent))
    case Exit(expr) => Exit(exprHandler(expr, current, parent))
    case Print(expr) => Print(exprHandler(expr, current, parent))
    case Println(expr) => Println(exprHandler(expr, current, parent))
    case WhileDo(condition, stmts) => WhileDo(
      exprHandler(condition, current, parent),
      scopeHandler(stmts, parent ++ current.toMap)
    )
    case IfElse(condition, thenStmts, elseStmts) => IfElse(
      exprHandler(condition, current, parent),
      scopeHandler(thenStmts, parent ++ current.toMap),
      scopeHandler(elseStmts, parent ++ current.toMap)
    )
    case Scope(stmts) => Scope(scopeHandler(stmts, parent ++ current.toMap))
}

def scopeHandler(
  stmts: List[Stmt],
  parent: Map[String, QualifiedName]
): List[Stmt] = {
  val current = mutable.Map.empty[String, QualifiedName]

  return stmts.map(renameStmt(_, current, parent))
}

def renameAssign(
  t: Type,
  name: String,
  rValue: RValue,
  current: mutable.Map[String,
  QualifiedName],
  parent: Map[String, QualifiedName]
  ): Stmt = {
  if current.contains(name) then {
    val newRValue = rValueHandler(rValue, current, parent)
    scopeErrors += s"Illegal redeclaration of variable $name"
    val newName = QualifiedName("", 0, Undefined)
    Assgn(Undefined, newName, newRValue)
  }

  val newRValue = rValueHandler(rValue, current, parent)
  globalNumberingUpdate(name)

  val newName = QualifiedName(name, globalNumbering(name), t)
  current(name) = newName
  return Assgn(t, newName, newRValue)
}

def exprHandler(
  expr: Expr,
  current: mutable.Map[String, QualifiedName],
  parent: Map[String, QualifiedName]
): Expr = {
  expr match
    case Ident(name) => renameIdent(name, current, parent)
    case ArrayElem(Ident(arrayName), index) => ArrayElem(
      renameIdent(arrayName, current, parent),
      index.map(exprHandler(_, current, parent))
    )
    case Neg(x) => Neg(exprHandler(x, current, parent))
    case Not(x) => Not(exprHandler(x, current, parent))
    case Len(x) => Len(exprHandler(x, current, parent))
    case Chr(x) => Chr(exprHandler(x, current, parent))
    case Ord(x) => Ord(exprHandler(x, current, parent))
    case Mul(l, r) => Mul(exprHandler(l, current, parent), exprHandler(r, current, parent))
    case Div(l, r) => Div(exprHandler(l, current, parent), exprHandler(r, current, parent))
    case Mod(l, r) => Mod(exprHandler(l, current, parent), exprHandler(r, current, parent))
    case Add(l, r) => Add(exprHandler(l, current, parent), exprHandler(r, current, parent))
    case Sub(l, r) => Sub(exprHandler(l, current, parent), exprHandler(r, current, parent))
    case Less(l, r) => Less(exprHandler(l, current, parent), exprHandler(r, current, parent))
    case LessE(l, r) => LessE(exprHandler(l, current, parent), exprHandler(r, current, parent))
    case Greater(l, r) => Greater(exprHandler(l, current, parent), exprHandler(r, current, parent))
    case GreaterE(l, r) => GreaterE(exprHandler(l, current, parent), exprHandler(r, current, parent))
    case Eq(l, r) => Eq(exprHandler(l, current, parent), exprHandler(r, current, parent))
    case NotEq(l, r) => NotEq(exprHandler(l, current, parent), exprHandler(r, current, parent))
    case And(l, r) => And(exprHandler(l, current, parent), exprHandler(r, current, parent))
    case Or(l, r) => Or(exprHandler(l, current, parent), exprHandler(r, current, parent))
    case literals: Expr => literals
}

def renameIdent(
  name: String,
  current: mutable.Map[String, QualifiedName],
  parent: Map[String, QualifiedName]
): QualifiedName = {
  if current.contains(name) then {
    return current(name)
  }
  if parent.contains(name) then {
    return parent(name)
  }
  scopeErrors += s"Variable $name has not been declared in scope"
  return QualifiedName(name, -1, Undefined)
}