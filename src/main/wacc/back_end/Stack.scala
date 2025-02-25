package wacc.back_end

import scala.collection.mutable
import wacc.front_end._
import wacc.back_end.Stack.typeToSize

object Stack {
  var frames: List[StackFrame] = Nil

  def initialise(stmts: List[Stmt]) = {
    frames = List(StackFrame(stmts))
  }
  
  def typeToSize(t: Type): Int = {
    t match 
      case PairType(t1, t2) => 8 // I think these
      case ArrayType(t, d) => 8 // are all 8
      case StringType => 8 // cuz they're pointers?
      case IntType => 4
      case BoolType => 1
      case CharType => 1
      case AnyType => ???
  }

  val intToMemOpModifier = Map(
    8 -> MemOpModifier.QWordPtr,
    4 -> MemOpModifier.DWordPtr,
    2 -> MemOpModifier.WordPtr,
    1 -> MemOpModifier.BytePtr
  )

}

case class StackFrame(stmts: List[Stmt]) {
  val identTable: mutable.Map[QualifiedName, Int] = mutable.Map()
  var currentDepth: Int = 1
    
  for (stmt <- stmts) {
    stmt match {
      case Assgn(t, qn: QualifiedName, rValue) =>
        identTable(qn) = currentDepth
        currentDepth += typeToSize(t)
      case _ => None
    }
  }
}