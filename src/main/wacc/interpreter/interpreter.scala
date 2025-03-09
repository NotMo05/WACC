package wacc.interpreter

import wacc.front_end._
import scala.collection.mutable
import scala.collection.immutable

enum PrintType:
  case PrintLn, Print

class Interpreter(prog: Prog) {
  val identTables: mutable.Stack[mutable.Map[QualifiedName, TypeOrPairElemValue]] = mutable.Stack()
  lazy val funcTable: immutable.Map[QualifiedFunc, Func] = // lazy as  functions may not be called
    prog.funcs.map {
      func => (func.identifier match
        case qf: QualifiedFunc => qf)
      -> func }.toMap()
  identTables.push(mutable.Map())

  def stmtsHandler(stmts: List[Stmt]): Any = stmts.tail.foldLeft(stmtHandler(stmts.head))((_, stmt) => stmtHandler(stmt))

  def execute(): Unit = stmtsHandler(prog.main)

  def stmtHandler(stmt: Stmt): Any = stmt match {
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
    case Skip =>
    case Read(_) => ???
    case Free(_) => ???
    case Exit(_) => ???
  }

  def reasgnHandler(lValue: LValue, rValue: RValue) = {
    val newRValue = rValueHandler(rValue)

    (lValue: @unchecked) match
      case qn: QualifiedName => identTables.top(qn) = newRValue
      case ArrayElem(arrayName: QualifiedName, indexes) => arrayLiterHandler(arrayName, indexes, newRValue)

      case Fst(lValue) => ???
      case Snd(lValue) => ???
  }

  def arrayLiterHandler(arrayName: QualifiedName, indexes: List[Expr], newRValue: TypeOrPairElemValue) = {
    (identTables.top(arrayName), indexes.map(evaluate(_))) match
      case (elems, (is: List[IntLiteral] @unchecked)) => {
        is.init.foldLeft(elems) { 
          case (ArrayBaseLiteral(elems), index: IntLiteral) => elems(index.int.toInt)
        } match
          case ArrayBaseLiteral(elems) => elems(is.last.int.toInt) = newRValue
      }
  }

  def rValueHandler(rValue: RValue): TypeOrPairElemValue = (rValue: @unchecked) match
    case expr: Expr => evaluate(expr)
    case Call(qf: QualifiedFunc, exprs: List[Expr]) => callHandler(qf, exprs)
    case Fst(_) => ???
    case Snd(_) => ???
    case ArrayLiter(exprs: List[Expr]) => ArrayBaseLiteral(exprs.map(evaluate(_)).toArray)
    case NewPair(_, _) => ???

  def assgnHandler: Assgn => Unit = {
    case Assgn(t, identifier, rValue) => {
      val result = rValueHandler(rValue)

      val qn = identifier match
        case qn: QualifiedName => qn

      identTables.top(qn) = result
    }
  }

  def callHandler(qf: QualifiedFunc, exprs: List[Expr]): TypeOrPairElemValue = {
    val func = funcTable(qf)
    val newIdentTable = (func.params.reverse.zip(exprs).map {
      (param, expr) => ((param.identifier: @unchecked) match
        case qn: QualifiedName => qn)
      -> evaluate(expr)
    }).to(mutable.Map)
    identTables.push(newIdentTable)
    val returnVal = stmtsHandler(func.stmts)
    identTables.pop()
    return returnVal match
      case x: TypeOrPairElemValue => x
  }

  def evaluate(expr: Expr): TypeOrPairElemValue = (expr: @unchecked) match {
    case int: IntLiteral => int
    case bool: BoolLiteral => bool
    case string: StringLiteral => string
    case char: CharLiteral => char
    case qn: QualifiedName => identTables.top(qn)
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
      case ArrayBaseLiteral(elems) => IntLiteral(elems.size)
    case Ord(expr) => (evaluate(expr): @unchecked) match
      case CharLiteral(char) => IntLiteral(char.toInt)
    case Chr(expr) => (evaluate(expr): @unchecked) match
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

    func(itemStringHandler(item))
  }

  def itemStringHandler(item: TypeOrPairElemValue): String = item match
    case IntLiteral(int) => int.toString()
    case BoolLiteral(bool) => bool.toString()
    case StringLiteral(string) => string
    case CharLiteral(char) => char.toString()
    case ArrayBaseLiteral(elems) => elems.map(itemStringHandler(_)).mkString("[", ", ", "]")

}