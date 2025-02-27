package wacc.back_end

import scala.collection.mutable
import wacc.front_end._
import wacc.back_end.Stack.typeToSize
import scala.collection.mutable.ListBuffer

object Stack {
  var frames: ListBuffer[StackFrame] = ListBuffer()

  def getOffset(qn: QualifiedName) = frames.last.identTable(qn)

  def initialise(stmts: List[Stmt]) = frames = ListBuffer(StackFrame(stmts, mutable.Map(), 0))
  
  def typeToSize(t: Type): Int = {
    (t: @unchecked) match 
      case PairType(t1, t2) => 8
      case ArrayType(t, d) => 8
      case StringType => 8
      case IntType => 4
      case BoolType => 1
      case CharType => 1
      case AnyType => ???
  }
}

case class StackFrame(stmts: List[Stmt], prevTable: mutable.Map[QualifiedName, Int], prevDepth: Int) {
  val identTable: mutable.Map[QualifiedName, Int] = prevTable
  var currentDepth: Int = 0
  def absoluteDepth(): Int = prevDepth + currentDepth
  for (stmt <- stmts) {
    stmt match {
      case Assgn(t, qn: QualifiedName, rValue) =>
        //Check order
        currentDepth -= typeToSize(t)
        identTable(qn) = absoluteDepth()
      case _ => None
    }
  }
}