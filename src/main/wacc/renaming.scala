
package wacc

import scala.collection.mutable

class QualifiedName(name: String, num: Int, val t:Type) extends Ident(name) {
}


val globalNumbering: mutable.Map[String, Int] = mutable.Map()

def rename(prog: Prog) : List[Stmt] = {
  return scopeHandler(prog.main, Map.empty[String, QualifiedName])
}



def lValueHandler(
  lValue: LValue, 
  current: mutable.Map[String, QualifiedName], 
  parent: Map[String, QualifiedName]
  ): LValue = {
  lValue match
    case Ident(identifier) => renameIdent(identifier, current, parent)
    case arrayElem: ArrayElem => exprHandler(arrayElem, current, parent).asInstanceOf[LValue]
    case Fst(lValue) => lValueHandler(lValue, current, parent)
    case Snd(lValue) => lValueHandler(lValue, current, parent)
}


def lValueString(
  lValue: LValue): String = {
  lValue match
    case Ident(identifier) => identifier
    case ArrayElem(arrayName, index) => arrayName.identifier
    case Fst(lValue) => lValueString(lValue)
    case Snd(lValue) => lValueString(lValue)
}

def renameReAssgn(lValue: LValue, rValue: RValue,
  current: mutable.Map[String, QualifiedName], 
  parent: Map[String, QualifiedName]
 ) = {
  val baseIdent = lValueString(lValue)
  if !(current.contains(baseIdent) || parent.contains(baseIdent)) then ??? // Error
  ReAssgn(lValueHandler(lValue, current, parent), rValueHandler(rValue, current, parent))
}

def rValueHandler(
  rValue: RValue,
  current: mutable.Map[String, QualifiedName],
  parent: Map[String, QualifiedName]
  ): RValue = {
    rValue match
      case expr: Expr => exprHandler(expr, current, parent)
      case Call(ident, args) => ??? //function stuff
      case Fst(lValue) => Fst(lValueHandler(lValue, current, parent))
      case Snd(lValue) => Snd(lValueHandler(lValue, current, parent))
      case ArrayLiter(elems) => ArrayLiter(elems.map(exprHandler(_, current, parent)))
      case NewPair(fst, snd) => NewPair(exprHandler(fst, current, parent), exprHandler(snd, current, parent))
  }

def renameStmt(
  stmt: Stmt, 
  current: mutable.Map[String, QualifiedName], 
  parent: Map[String, QualifiedName]
): Stmt = {
  stmt match
    case Skip => Skip 
    case Assgn(t, Ident(name), rValue) => renameAssign(t, name, rValue, current)
    case ReAssgn(lValue, rValue) => renameReAssgn(lValue, rValue, current, parent)
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

def renameAssign(t: Type, name: String, rValue: RValue, current: mutable.Map[String, QualifiedName]): Stmt= {
  if current.contains(name) then ??? // Error

  globalNumbering.updateWith(name) {
    case Some(n) => Some(n + 1)
    case None => Some(0)
  }

  val newName = QualifiedName(name, globalNumbering(name), t)
  current(name) = newName
  return Assgn(t, newName, rValue)
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
  ??? //TODO: Error
}

 