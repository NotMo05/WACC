package wacc.interpreter

import wacc.front_end._
import scala.collection.mutable
import scala.collection.immutable
import cats.implicits._

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

  def stmtsHandler(stmts: List[Stmt]): Option[TypeOrPairElemValue] =
    stmts.collectFirst { case stmt if stmtHandler(stmt).isDefined => stmtHandler(stmt).get }

  def execute(): Unit = stmtsHandler(prog.main)

  def stmtHandler(stmt: Stmt): Option[TypeOrPairElemValue] = stmt match {
    case Return(expr) => evaluate(expr)
    case Print(expr) => printHandler(evaluate(expr), PrintType.Print)
    case Println(expr) => printHandler(evaluate(expr), PrintType.PrintLn)
    case WhileDo(condition, stmts) => {
      var ret: Option[TypeOrPairElemValue] = None
      while (ret.isEmpty && evaluate(condition).collect {
        case BoolLiteral(bool: Boolean) => bool
      }.getOrElse(throw new RuntimeException("Somehow the expr did not evaluate")))
      {
        ret = stmtsHandler(stmts)  // Capture the return value
      }
      ret
    }
    case IfElse(condition, thenStmts, elseStmts) => {
      evaluate(condition) match {
        case Some(BoolLiteral(bool)) =>
          if bool then stmtsHandler(thenStmts)
          else stmtsHandler(elseStmts)
        case _ =>
          // Handle unexpected cases, e.g., None or incorrect types
          throw new RuntimeException("Expected BoolLiteral but got None or wrong type")
      }
    }

    case Scope(stmts) => stmtsHandler(stmts) // No need to create new stack, renaming already handled
    case assgn: Assgn => assgnHandler(assgn)
    case ReAssgn(lValue, rValue) => reasgnHandler(lValue, rValue)
    case Skip => None
    case Read(_) => ???
    case Free(_) => ???
    case Exit(_) => ???
  }

  def reasgnHandler(lValue: LValue, rValue: RValue): Option[TypeOrPairElemValue] = {
    val newRValue = rValueHandler(rValue)

    (lValue: @unchecked) match
      case qn: QualifiedName => identTables.top(qn) = newRValue.getOrElse(throw new RuntimeException("Somehow the expr did not evaluate")); None
      case ArrayElem(arrayName: QualifiedName, indexes) => arrayLiterAssgnHandler(arrayName, indexes, newRValue)

      case Fst(lValue) => ???
      case Snd(lValue) => ???
  }

  def resolveArrayElem(arrayName: QualifiedName, indexes: List[Expr]): (ArrayBaseLiteral, Int) = {
    (identTables.top(arrayName), indexes.map(evaluate(_))) match
      case (elems, (is: List[IntLiteral] @unchecked)) =>
        is.init.foldLeft(elems) {
          case (ArrayBaseLiteral(elems), index: IntLiteral) => elems(index.int.toInt)
        } match {
          case arr @ ArrayBaseLiteral(elems) => (
            arr,
            is.last match {
              case Some(IntLiteral(int)) => int.toInt
              case None => throw new RuntimeException("Somehow the expr did not evaluate")
            })
        }
  }

  def arrayLiterAssgnHandler(arrayName: QualifiedName, indexes: List[Expr], newRValue: Option[TypeOrPairElemValue]) = {
    val (arrayBase, lastIndex) = resolveArrayElem(arrayName, indexes)
    arrayBase.elems(lastIndex) = newRValue.getOrElse(throw new RuntimeException("Somehow the expr did not evaluate"))
    None
  }

  def arrayLiterAccessHandler(arrayName: QualifiedName, indexes: List[Expr]): TypeOrPairElemValue = {
    val (arrayBase, lastIndex) = resolveArrayElem(arrayName, indexes)
    arrayBase.elems(lastIndex)
  }

  def rValueHandler(rValue: RValue): Option[TypeOrPairElemValue] = (rValue: @unchecked) match
    case expr: Expr => evaluate(expr)
    case Call(qf: QualifiedFunc, exprs: List[Expr]) => callHandler(qf, exprs)
    case Fst(_) => ???
    case Snd(_) => ???
    case ArrayLiter(exprs: List[Expr]) =>  exprs.traverse(evaluate).map(elems => ArrayBaseLiteral(elems.toArray))
    case NewPair(_, _) => ???

  def assgnHandler: Assgn => Option[TypeOrPairElemValue] = {
    case Assgn(t, identifier, rValue) => {
      val result = rValueHandler(rValue).getOrElse(throw new RuntimeException("Somehow the expr did not evaluate"))

      val qn = identifier match
        case qn: QualifiedName => qn

      identTables.top(qn) = result
      None
    }
  }

  def callHandler(qf: QualifiedFunc, exprs: List[Expr]): Option[TypeOrPairElemValue] = {
    val func = funcTable(qf)
    val newIdentTable = (func.params.reverse.zip(exprs).map {
      (param, expr) => ((param.identifier: @unchecked) match
        case qn: QualifiedName => qn)
      -> evaluate(expr).getOrElse(throw new RuntimeException("Somehow the expr did not evaluate"))
    }).to(mutable.Map)
    identTables.push(newIdentTable)
    val returnVal = stmtsHandler(func.stmts)
    identTables.pop()
    return returnVal
  }

  def evaluate(expr: Expr): Option[TypeOrPairElemValue] = (expr: @unchecked) match {
    case int: IntLiteral => Some(int)
    case bool: BoolLiteral => Some(bool)
    case string: StringLiteral => Some(string)
    case char: CharLiteral => Some(char)
    case qn: QualifiedName => Some(identTables.top(qn))
    case ArrayElem(arrayName: Ident, indexes: List[Expr]) =>
      arrayName match
        case arrayName: QualifiedName => Some(arrayLiterAccessHandler(arrayName, indexes))
    case NullLiteral => ???

    case unaryOp: UnaryOperator => unaryEvaluator(unaryOp)
    case binaryOp: BinaryOperator => binEvaluator(binaryOp)
  }

  def unaryEvaluator(unaryOp: UnaryOperator): Option[TypeOrPairElemValue] = unaryOp match {
    case Neg(expr) => evaluate(expr) match {
      case Some(IntLiteral(int)) => Some(IntLiteral(-int))
      case _ => None
    }

    case Not(expr) => evaluate(expr) match {
      case Some(BoolLiteral(bool)) => Some(BoolLiteral(!bool))
      case _ => None
    }

    case Len(expr) => evaluate(expr) match {
      case Some(StringLiteral(str)) => Some(IntLiteral(str.length))
      case Some(ArrayBaseLiteral(elems)) => Some(IntLiteral(elems.size))
      case _ => None
    }

    case Ord(expr) => evaluate(expr) match {
      case Some(CharLiteral(char)) => Some(IntLiteral(char.toInt))
      case _ => None
    }

    case Chr(expr) => evaluate(expr) match {
      case Some(IntLiteral(int)) => Some(CharLiteral(int.toChar))
      case _ => None
    }
  }

  def binEvaluator(binaryOp: BinaryOperator): Option[TypeOrPairElemValue] = {
    def evalBinaryArithmeticOp(
      l: Expr, r: Expr, op: (BigInt, BigInt) => BigInt
      ): Option[TypeOrPairElemValue] = {
      evalBinaryOp(l, r) match {
        case (Some(IntLiteral(a: BigInt)), Some(IntLiteral(b: BigInt))) => Some(IntLiteral(op(a, b)))
        case _ => throw new IllegalArgumentException("Invalid operands for binary operation")
      }
    }

    def evalBinaryOp(l: Expr, r: Expr): (Option[TypeOrPairElemValue], Option[TypeOrPairElemValue]) = (evaluate(l), evaluate(r))

    def evalBinaryLogicalOp(
        l: Expr, r: Expr, op: (Boolean, Boolean) => Boolean
      ): Option[TypeOrPairElemValue] = {
        evalBinaryOp(l, r) match {
          case (Some(BoolLiteral(a: Boolean)), Some(BoolLiteral(b: Boolean))) => Some(BoolLiteral(op(a, b)))
          case _ => throw new IllegalArgumentException("Invalid operands for binary operation")
        }
    }

    binaryOp match
      case Mul(l: Expr, r: Expr) => evalBinaryArithmeticOp(l, r, (a, b) => a * b)
      case Div(l: Expr, r: Expr) => evalBinaryArithmeticOp(l, r, (a, b) => a / b)
      case Mod(l: Expr, r: Expr) => evalBinaryArithmeticOp(l, r, (a, b) => a % b)
      case Add(l: Expr, r: Expr) => evalBinaryArithmeticOp(l, r, (a, b) => a + b)
      case Sub(l: Expr, r: Expr) => evalBinaryArithmeticOp(l, r, (a, b) => a - b)

      case Less(l: Expr, r: Expr) => (evalBinaryOp(l, r): @unchecked) match
        case (Some(IntLiteral(a)), Some(IntLiteral(b))) => Some(BoolLiteral(a < b))
        case (Some(CharLiteral(a)), Some(CharLiteral(b))) => Some(BoolLiteral(a < b))

      case LessE(l: Expr, r: Expr) => (evalBinaryOp(l, r): @unchecked) match
        case (Some(IntLiteral(a)), Some(IntLiteral(b))) => Some(BoolLiteral(a <= b))
        case (Some(CharLiteral(a)), Some(CharLiteral(b))) => Some(BoolLiteral(a <= b))

      case Greater(l: Expr, r: Expr) => (evalBinaryOp(l, r): @unchecked) match
        case (Some(IntLiteral(a)), Some(IntLiteral(b))) => Some(BoolLiteral(a > b))
        case (Some(CharLiteral(a)), Some(CharLiteral(b))) => Some(BoolLiteral(a > b))

      case GreaterE(l: Expr, r: Expr) => (evalBinaryOp(l, r): @unchecked) match
        case (Some(IntLiteral(a)), Some(IntLiteral(b))) => Some(BoolLiteral(a >= b))
        case (Some(CharLiteral(a)), Some(CharLiteral(b))) => Some(BoolLiteral(a >= b))

      case Eq(l: Expr, r: Expr) => (evalBinaryOp(l, r): @unchecked) match
        case (Some(IntLiteral(a)), Some(IntLiteral(b))) => Some(BoolLiteral(a == b))
        case (Some(CharLiteral(a)), Some(CharLiteral(b))) => Some(BoolLiteral(a == b))
        case (Some(BoolLiteral(a)), Some(BoolLiteral(b))) => Some(BoolLiteral(a == b))
        case (Some(ArrayBaseLiteral(es1)), Some(ArrayBaseLiteral(es2))) => Some(BoolLiteral(es1 eq es2)) //check referential equality

      case NotEq(l: Expr, r: Expr) => (evalBinaryOp(l, r): @unchecked) match
        case (Some(IntLiteral(a)), Some(IntLiteral(b))) => Some(BoolLiteral(a != b))
        case (Some(CharLiteral(a)), Some(CharLiteral(b))) => Some(BoolLiteral(a != b))
        case (Some(BoolLiteral(a)), Some(BoolLiteral(b))) => Some(BoolLiteral(a != b))
        case (Some(ArrayBaseLiteral(es1)), Some(ArrayBaseLiteral(es2))) => Some(BoolLiteral((es1 ne es2))) //check referential equality

      case And(l: Expr, r: Expr) => evalBinaryLogicalOp(l, r, (a, b) => a && b)
      case Or(l: Expr, r: Expr) => evalBinaryLogicalOp(l, r, (a, b) => a || b)
  }

  def printHandler(item: Option[TypeOrPairElemValue], printType: PrintType): Option[TypeOrPairElemValue] = {
    val func: (Any => Unit) = printType match
      case PrintType.PrintLn => println(_)
      case PrintType.Print => print(_)

    func(itemStringHandler(item))
    None
  }

  def itemStringHandler(item: Option[TypeOrPairElemValue]): String = item match
    case Some(IntLiteral(int)) => int.toString()
    case Some(BoolLiteral(bool)) => bool.toString()
    case Some(StringLiteral(string)) => string
    case Some(CharLiteral(char)) => char.toString()
    case Some(ArrayBaseLiteral(elems)) =>
      elems.collect { case CharLiteral(c) => c }.mkString match {
        case "" => s"0x${System.identityHashCode(elems).toHexString}" // If no CharLiterals are found
        case str => str
      }
    case None => "None"  // Handle the None case

}