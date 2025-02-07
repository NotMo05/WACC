package wacc

import scala.collection.mutable

class QualifiedName(val name: String, val num: Int, val t:Type) extends Ident(name) {
  override def toString() = {s"QualifiedName(name=$name, num=$num, t=$t)"}
}

class QualifiedFunc(val t: Type, funcName: String, val paramNum: Int, val paramTypes: List[Type]) extends Ident(funcName) {
  override def toString() = {s"QualifiedFunc(t=$t, funcName=$funcName, paramNum=$paramNum, paramTypes=$paramTypes)"}
}

val funcTypes: mutable.Map[String, QualifiedFunc] = mutable.Map()

val globalNumbering: mutable.Map[String, Int] = mutable.Map()

val scopeErrors = List.newBuilder[String]

def globalNumberingUpdate(name: String): Option[Int] = {
  globalNumbering.updateWith(name) {
    case Some(n) => Some(n + 1)
    case None => Some(0)
  }
}

def rename(prog: Prog) : (Prog, List[String]) = {
  scopeErrors.clear()
  globalNumbering.clear()
  funcTypes.clear()
  funcFirstPass(prog.funcs)
  return (
    Prog(prog.funcs.map(funcHandler(_)),
    scopeHandler(prog.main, Map.empty[String, QualifiedName])),
    scopeErrors.result()
    )
}

def funcFirstPass(funcs: List[Func]) = {
  for (func <- funcs) {
    val name = func.identifier.identifier
    if funcTypes.contains(name) then scopeErrors += s"Illegal redefinition of function $name"
    funcTypes(name) = QualifiedFunc(func.t, name, func.params.size, func.params.map(_.t))
  }
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

  val name = func.identifier.identifier
  val params = func.params
  val paramScope = params.reverse.map(paramHandler(_)).distinctBy(_._1).toMap[String, QualifiedName]
  val qParam = paramScope.map((_,qn) => Param(qn.t, qn)).toList
  Func(func.t, funcTypes(name), qParam, scopeHandler(func.stmts, paramScope))
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


def lValueString(lValue: LValue): String = {
  lValue match
    case Ident(identifier) => identifier
    case ArrayElem(arrayName, index) => arrayName.identifier
    case Fst(lValue) => lValueString(lValue)
    case Snd(lValue) => lValueString(lValue)
}

def rValueHandler(
  rValue: RValue,
  current: mutable.Map[String, QualifiedName],
  parent: Map[String, QualifiedName]
  ): RValue = {
    rValue match
      case expr: Expr => exprHandler(expr, current, parent)
      case Call(ident, args) if funcTypes.contains(ident.identifier) => 
        Call(funcTypes(ident.identifier), args.map(exprHandler(_, current, parent)))

      case Fst(lValue) => Fst(lValueHandler(lValue, current, parent))
      case Snd(lValue) => Snd(lValueHandler(lValue, current, parent))
      case ArrayLiter(elems) => ArrayLiter(elems.map(exprHandler(_, current, parent)))
      case NewPair(fst, snd) =>
        NewPair(exprHandler(fst, current, parent), exprHandler(snd, current, parent))
      case c: Call => {
        scopeErrors += s"Function ${c.ident} has not been defined"
        Call(QualifiedFunc(Undefined, "", 0, List()), List())
      }
    }

def renameStmt(
  stmt: Stmt,
  current: mutable.Map[String, QualifiedName],
  parent: Map[String, QualifiedName]
): Stmt = {
  stmt match
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
    scopeErrors += s"Illegal redeclaration of variable $name"
    val newName = QualifiedName("", 0, Undefined)
    Assgn(Undefined, newName, rValueHandler(rValue, current, parent))
  }

  globalNumberingUpdate(name)

  val newName = QualifiedName(name, globalNumbering(name), t)
  current(name) = newName
  return Assgn(t, newName, rValueHandler(rValue, current, parent))
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
  return QualifiedName("", 0, Undefined)
}
