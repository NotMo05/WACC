package wacc.back_end

import scala.collection.mutable
import wacc.front_end._
import wacc.back_end.Stack.typeToSize
import scala.collection.mutable.ListBuffer

/**
 * The `Stack` object represents a stack of frames used in the back-end of the WACC compiler.
 * It provides methods to manage stack frames and calculate offsets and sizes for different types.
 * 
 * - `frames`: A mutable list buffer that holds the stack frames.
 * - `getOffset(qn: QualifiedName)`: Retrieves the offset for a given qualified name from the last stack frame.
 * - `initialise(stmts: List[Stmt])`: Initializes the stack with a single stack frame containing the given statements.
 * - `typeToSize(t: Type): Int`: Returns the size in bytes for a given type. The size is determined based on the type:
 *   - `PairType`, `ArrayType`, `StringType`: 8 bytes
 *   - `IntType`: 4 bytes
 *   - `BoolType`, `CharType`: 1 byte
 *   - `AnyType`: Not yet implemented (throws an exception)
*/
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
  }
}

/**
 * This class represents stack frame in the WACC backend.
 *
 * @param stmts A list of statements to be processed in this stack frame.
 * @param prevTable A mutable map representing the previous identifier table with qualified names and their corresponding depths.
 * @param prevDepth The depth of the previous stack frame.
 *
 * @constructor Creates a new StackFrame with the given statements, previous identifier table, and previous depth.
 *
 * @val identTable A mutable map representing the current identifier table, initialized with the previous identifier table.
 * @var currentDepth The current depth of the stack frame, initialized to 0.
 *
 * @method absoluteDepth Calculates the absolute depth of the current stack frame by adding the previous depth and the current depth.
 *
 * The constructor processes each statement in the list of statements. If the statement is an assignment, it updates the current depth
 * by subtracting the size of the type being assigned and updates the identifier table with the absolute depth of the qualified name.
*/
case class StackFrame(stmts: List[Stmt], prevTable: mutable.Map[QualifiedName, Int], prevDepth: Int) {
  val identTable: mutable.Map[QualifiedName, Int] = prevTable
  var currentDepth: Int = 0
  def absoluteDepth(): Int = prevDepth + currentDepth
  for (stmt <- stmts) {
    stmt match {
      case Assgn(t, qn: QualifiedName, rValue) =>
        currentDepth -= typeToSize(t)
        identTable(qn) = absoluteDepth()
      case _ => None
    }
  }
}