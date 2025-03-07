package wacc.interpreter

import wacc.front_end._
import scala.collection.mutable
import scala.collection.immutable

enum PrintType:
  case PrintLn, Print

class Interpreter(prog: Prog) {
  val identTable: mutable.Stack[mutable.Map[QualifiedName, TypeOrPairElemValue]] = mutable.Stack()
  lazy val funcTable: immutable.Map[QualifiedFunc, Func] =
    prog.funcs.map {
      func => (func.identifier match
        case qf: QualifiedFunc => qf)
      -> func }.toMap()
  identTable.push(mutable.Map())

  def stmtsHandler(stmts: List[Stmt]): Unit = stmts.map(stmtHandler(_))

  def execute(): Unit = stmtsHandler(prog.main)

  def stmtHandler(stmt: Stmt): Unit = stmt match {
    case Return(expr) => evaluate(expr)
    case Print(expr) => printHandler(evaluate(expr), PrintType.Print)
    case Println(expr) => printHandler(evaluate(expr), PrintType.PrintLn)
    case WhileDo(condition, stmts) => {
      while ((evaluate(condition): @unchecked) match
        case BoolLiteral(bool) =>  bool) {
          stmtsHandler(stmts)
        }
    }
    case IfElse(condition, thenStmts, elseStmts) => {
      (evaluate(condition): @unchecked) match
        case BoolLiteral(bool) => {
          if bool then stmtsHandler(thenStmts)
          else stmtsHandler(elseStmts)
        }
    }

    case Scope(stmts) => stmtsHandler(stmts) // No need to create new stack, renaming already handled
    case assgn: Assgn => assgnHandler(assgn)
    case ReAssgn(lValue, rValue) => reasgnHandler(lValue, rValue)
    case Skip => ???
    case Read(_) => ???
    case Free(_) => ???
    case Exit(_) => ???
  }

  def reasgnHandler(lValue: LValue, rValue: RValue) = {
    val newRValue = rValueHandler(rValue)

    (lValue: @unchecked) match
      case qn: QualifiedName => identTable.top(qn) = newRValue
      case ArrayElem(arrayName, index) => ???
      case Fst(lValue) => ???
      case Snd(lValue) => ???
  }

  def rValueHandler(rValue: RValue): TypeOrPairElemValue = (rValue: @unchecked) match
    case expr: Expr => evaluate(expr)
    case Call(qf: QualifiedFunc, exprs: List[Expr]) => callHandler(qf, exprs)
    case Fst(_) => ???
    case Snd(_) => ???
    case ArrayLiter(_) => ???
    case NewPair(_, _) => ???

  def assgnHandler: Assgn => Unit = {
    case Assgn(t, identifier, rValue) => {
      val result = rValueHandler(rValue)

      val qn = identifier match
        case qn: QualifiedName => qn

      identTable.top(qn) = result
    }
  }

  def callHandler(qf: QualifiedFunc, exprs: List[Expr]): TypeOrPairElemValue = {
    ???
  }

  def evaluate(expr: Expr): TypeOrPairElemValue = (expr: @unchecked) match {
    case int: IntLiteral => int
    case bool: BoolLiteral => bool
    case string: StringLiteral => string
    case char: CharLiteral => char
    case qn: QualifiedName => identTable.top(qn)
    case ArrayElem(arrayName: Ident, index: List[Expr]) => ???
    case NullLiteral => ???

    case unaryOp: UnaryOperator => unaryEvaluator(unaryOp)
    case binaryOp: BinaryOperator => binEvaluator(binaryOp)
  }

  def unaryEvaluator(unaryOp: UnaryOperator): TypeOrPairElemValue = unaryOp match {
    case Neg(expr) => (evaluate(expr): @unchecked) match
      case IntLiteral(int) => IntLiteral(-int)
    case Not(expr) => (evaluate(expr): @unchecked) match
      case BoolLiteral(bool) => BoolLiteral(!bool)
    case Len(expr) => (evaluate(expr): @unchecked) match
      case StringLiteral(str) => IntLiteral(str.length)
    case Chr(expr) => (evaluate(expr): @unchecked) match
      case CharLiteral(char) => IntLiteral(char.toInt)
    case Ord(expr) => (evaluate(expr): @unchecked) match
      case IntLiteral(int) => CharLiteral(int.toChar)
  }

  def binEvaluator(binaryOp: BinaryOperator): TypeOrPairElemValue = {
    def evalBinaryArithmeticOp(
      l: Expr, r: Expr, op: (BigInt, BigInt) => BigInt
      ): TypeOrPairElemValue = {
      evalBinaryOp(l, r) match {
        case (IntLiteral(a: BigInt), IntLiteral(b: BigInt)) => IntLiteral(op(a, b))
        case _ => throw new IllegalArgumentException("Invalid operands for binary operation")
      }
    }

    def evalBinaryOp(l: Expr, r: Expr): (TypeOrPairElemValue, TypeOrPairElemValue) = (evaluate(l), evaluate(r))

    def evalBinaryLogicalOp(
      l: Expr, r: Expr, op: (Boolean, Boolean) => Boolean
      ): TypeOrPairElemValue = {
      evalBinaryOp(l, r) match {
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

      case Less(l, r) => (evalBinaryOp(l, r): @unchecked) match
        case (IntLiteral(a), IntLiteral(b)) => BoolLiteral(a < b)
        case (CharLiteral(a), CharLiteral(b)) => BoolLiteral(a < b)

      case LessE(l, r) => (evalBinaryOp(l, r): @unchecked) match
        case (IntLiteral(a), IntLiteral(b)) => BoolLiteral(a <= b)
        case (CharLiteral(a), CharLiteral(b)) => BoolLiteral(a <= b)

      case Greater(l, r) => (evalBinaryOp(l, r): @unchecked) match
        case (IntLiteral(a), IntLiteral(b)) => BoolLiteral(a > b)
        case (CharLiteral(a), CharLiteral(b)) => BoolLiteral(a > b)

      case GreaterE(l, r) => (evalBinaryOp(l, r): @unchecked) match
        case (IntLiteral(a), IntLiteral(b)) => BoolLiteral(a >= b)
        case (CharLiteral(a), CharLiteral(b)) => BoolLiteral(a >= b)

      case Eq(l, r) => (evalBinaryOp(l, r): @unchecked) match
        case (IntLiteral(a), IntLiteral(b)) => BoolLiteral(a == b)
        case (CharLiteral(a), CharLiteral(b)) => BoolLiteral(a == b)
        case (BoolLiteral(a), BoolLiteral(b)) => BoolLiteral(a == b)

      case NotEq(l, r) => (evalBinaryOp(l, r): @unchecked) match
        case (IntLiteral(a), IntLiteral(b)) => BoolLiteral(a != b)
        case (CharLiteral(a), CharLiteral(b)) => BoolLiteral(a != b)
        case (BoolLiteral(a), BoolLiteral(b)) => BoolLiteral(a != b)

      case And(l, r) => evalBinaryLogicalOp(l, r, (a, b) => a && b)
      case Or(l, r) => evalBinaryLogicalOp(l, r, (a, b) => a || b)
  }

  def printHandler(item: TypeOrPairElemValue, printType: PrintType) = {
    val func: (Any => Unit) = printType match
      case PrintType.PrintLn => println(_)
      case wacc.interpreter.PrintType.Print => print(_)

    item match
      case IntLiteral(int) => func(int)
      case BoolLiteral(bool) => func(bool)
      case StringLiteral(string) => func(string)
      case CharLiteral(char) => func(char)
      case ArrayElem(arrayName, index) => ???
  }
}