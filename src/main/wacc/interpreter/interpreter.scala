package wacc.interpreter

import wacc.front_end._
import wacc.front_end.Stmt
import wacc.front_end.QualifiedName
import wacc.front_end.TypeOrPairElemValue
import scala.collection.mutable

object Interpreter {
  val identTable: mutable.Map[QualifiedName, TypeOrPairElemValue] = mutable.Map()

  def execute(prog: Prog): Unit = {
    prog.main.map(stmtHandler(_))
  }

  def stmtHandler(stmt: Stmt): Unit = stmt match {
    case Return(expr) => ???
    case Print(expr) => printHandler(evaluate(expr))
    case Println(expr) => printlnHandler(evaluate(expr))
    case WhileDo(condition, stmts) => ???
    case IfElse(condition, thenStmts, elseStmts) => {
      evaluate(condition: @unchecked) match
        case BoolLiteral(bool) => {
          if bool then thenStmts.map(stmtHandler(_))
          else elseStmts.map(stmtHandler(_))
        }
    }

    case Scope(stmts) => ???
    case Assgn(t, identifier, rValue) => ???
    case ReAssgn(lValue, rValue) => ???
    case _ => ???
  }

  def evaluate(expr: Expr): TypeOrPairElemValue = expr match {
    case int: IntLiteral => int
    case bool: BoolLiteral => bool
    case string: StringLiteral => string
    case char: CharLiteral => char
    case Ident(identifier: String) => ???
    case ArrayElem(arrayName: Ident, index: List[Expr]) => ???
    case NullLiteral => ???

    case Neg(x: Expr) => ???
    case Not(x: Expr) => ???
    case Len(x: Expr) => ???
    case Chr(x: Expr) => ???
    case Ord(x: Expr) => ???

    case unaryOp: UnaryOperator => unaryEvaluator(unaryOp)
    case binaryOp: BinaryOperator => binEvaluator(binaryOp)
  }

  def unaryEvaluator(unaryOp: UnaryOperator): TypeOrPairElemValue = unaryOp match {
    case Neg(x) => ???
    case Not(x) => ???
    case Len(x) => ???
    case Chr(x) => ???
    case Ord(x) => ???
  }

  def binEvaluator(binaryOp: BinaryOperator): TypeOrPairElemValue = {
    def evalBinaryArithmeticOp(
      l: Expr, r: Expr, op: (BigInt, BigInt) => BigInt
      ): TypeOrPairElemValue = {
      (evaluate(l), evaluate(r)) match {
        case (IntLiteral(a: BigInt), IntLiteral(b: BigInt)) => IntLiteral(op(a, b))
        case _ => throw new IllegalArgumentException("Invalid operands for binary operation")
      }
    }

    def evalBinaryCompOp(
      l: Expr, r: Expr, op: (BigInt, BigInt) => Boolean
      ): TypeOrPairElemValue = {
      (evaluate(l), evaluate(r)) match {
        case (IntLiteral(a: BigInt), IntLiteral(b: BigInt)) => BoolLiteral(op(a, b))
        case _ => throw new IllegalArgumentException("Invalid operands for binary operation")
      }
    }

    def evalBinaryLogicalOp(
      l: Expr, r: Expr, op: (Boolean, Boolean) => Boolean
      ): TypeOrPairElemValue = {
      (evaluate(l), evaluate(r)) match {
        case (BoolLiteral(a: Boolean), BoolLiteral(b: Boolean)) => BoolLiteral(op(a, b))
        case _ => throw new IllegalArgumentException("Invalid operands for binary operation")
      }
    }

    binaryOp match
      case Mul(l, r) => evalBinaryArithmeticOp(l, r, (a, b) => a * b)
      case Div(l, r) => evalBinaryArithmeticOp(l, r, (a, b) => a / b)
      case Mod(l, r) => evalBinaryArithmeticOp(l, r, (a, b) => a % b)
      case Add(l, r) => evalBinaryArithmeticOp(l, r, (a, b) => a + b)
      case Sub(l, r) => evalBinaryArithmeticOp(l, r, (a, b) => a - b)

      case Less(l, r) => evalBinaryCompOp(l, r, (a, b) => a < b)
      case LessE(l, r) => evalBinaryCompOp(l, r, (a, b) => a <= b)
      case Greater(l, r) => evalBinaryCompOp(l, r, (a, b) => a > b)
      case GreaterE(l, r) => evalBinaryCompOp(l, r, (a, b) => a >= b)
      case Eq(l, r) => evalBinaryCompOp(l, r, (a, b) => a == b)
      case NotEq(l, r) => evalBinaryCompOp(l, r, (a, b) => a != b)
      case And(l, r) => evalBinaryLogicalOp(l, r, (a, b) => a && b)
      case Or(l, r) => evalBinaryLogicalOp(l, r, (a, b) => a || b)
  }

  def printlnHandler(item: TypeOrPairElemValue) = printHandler(item); print("\n")

  def printHandler(item: TypeOrPairElemValue) = item match
    case IntLiteral(int) => print(int)
    case BoolLiteral(bool) => print(bool)
    case StringLiteral(string) => print(string)
    case CharLiteral(char) => print(char)
    case ArrayElem(arrayName, index) => ???

  def exprHandler(expr: Expr): String = expr match {
    case IntLiteral(int: BigInt) => ???
    case BoolLiteral(bool: Boolean) => ???
    case StringLiteral(string: String) => string
    case CharLiteral(char: Char) => ???
    case Ident(identifier: String) => ???
    case ArrayElem(arrayName: Ident, index: List[Expr]) => ???
    case NullLiteral => ???

    case Neg(x: Expr) => ???
    case Not(x: Expr) => ???
    case Len(x: Expr) => ???
    case Chr(x: Expr) => ???
    case Ord(x: Expr) => ???

    case Mul(l: Expr, r: Expr) => ???

    case Div(l: Expr, r: Expr) => ???
    case Mod(l: Expr, r: Expr) => ???
    case Add(l: Expr, r: Expr) => ???
    case Sub(l: Expr, r: Expr) => ???
    case Less(l: Expr, r: Expr) => ???
    case LessE(l: Expr, r: Expr) => ???
    case Greater(l: Expr, r: Expr) => ???
    case GreaterE(l: Expr, r: Expr) => ???
    case Eq(l: Expr, r: Expr) => ???
    case NotEq(l: Expr, r: Expr) => ???
    case And(l: Expr, r: Expr) => ???
    case Or(l: Expr, r: Expr) => ???

  }
}