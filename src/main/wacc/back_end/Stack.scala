package wacc.back_end

import scala.collection.mutable
import wacc.front_end._
import wacc.back_end.Stack.typeToSize

object Stack {
  var frames: List[StackFrame] = Nil

  def getOffset(qn: QualifiedName) = frames.last.identTable(qn)

  def initialise(stmts: List[Stmt]) = frames = List(StackFrame(stmts))
  
  
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

case class StackFrame(stmts: List[Stmt]) {
  val identTable: mutable.Map[QualifiedName, Int] = mutable.Map()
  var currentDepth: Int = 0
    
  for (stmt <- stmts) {
    stmt match {
      case Assgn(t, qn: QualifiedName, rValue) =>
        //Check order
        currentDepth -= typeToSize(t)
        identTable(qn) = currentDepth
      case _ => None
    }
  }
}