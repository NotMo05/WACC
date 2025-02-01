package wacc

import wacc.syntax._
import scala.collection.mutable

object semantic {
  // Symbol table: Map each identifier to its declared type
  val symbolTable: mutable.Map[String, Type] = mutable.Map()

  def analyze(prog: Prog): Unit = {
    // Populate symbolTable for each Func param/return type
    prog.funcs.foreach(f => f.params.foreach(p => symbolTable(p.identifier.identifier) = p.t))

    // Check statements in the main program body
    prog.main.foreach(s => analyzeStmt(s))
  }

  private def analyzeStmt(stmt: Stmt): Unit = stmt match {
    case Assgn(t, ident, rVal) =>
      // Ensure ident not already declared or handle redeclaration
      symbolTable(ident.identifier) = t
      val rValType = getRValueType(rVal)
      if (rValType != t) {
        throw new Exception(s"Type mismatch: expected $t but got $rValType")
      }
    case ReAssgn(lVal, rVal) =>
      // Check the LValue type from symbolTable
      val lValType = getLValueType(lVal)
      val rValType = getRValueType(rVal)
      if (lValType != rValType) {
        throw new Exception(s"Type mismatch: expected $lValType but got $rValType")
      }
    case _ => ()
  }

  private def getLValueType(lVal: LValue): Type = lVal match {
    case Ident(name) =>
      // Look up declared type of `name` in symbolTable
      symbolTable.getOrElse(name, throw new Exception(s"Undeclared identifier: $name"))

    case ArrayElem(arrayName, _) =>
      // Look up array type in symbolTable, then return the element type
      // Possibly something like: if declared type is ArrayType(t, _) => t
      NoType

    case PairElem(pos, subLVal) =>
      // If declared type is PairType(t1, t2), choose based on `pos == Fst or Snd`
      NoType
  }

  private def getRValueType(rVal: RValue): Type = rVal match {
    case IntLiteral(_)    => IntType
    case BoolLiteral(_)   => BoolType
    case StringLiteral(_) => StringType
    case CharLiteral(_)   => CharType
    case Ident(name)      => symbolTable.getOrElse(name, NoType)
    case Add(l, r) =>
      // Both sides must be IntType; return IntType
      NoType
    // Handle other Expr nodes similarly (Sub, Mul, etc.)
    case _ => NoType
  }
}