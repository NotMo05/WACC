package wacc.interpreter

import wacc.front_end._
import scala.collection.mutable
import scala.collection.immutable
import cats.implicits._

case class ExitException(code: Option[TypeOrPairElemValue]) extends RuntimeException
case class FailedToEvaluate(message: String) extends RuntimeException

enum PrintType:
  case PrintLn, Print

final val exprCouldntEval = throw new FailedToEvaluate("Somehow the expr did not evaluate")
final val couldNotFindOnTable = throw new IllegalStateException("Somehow could not find the qn on the table")
final val nullAccess = throw new IllegalArgumentException("Atempted to access a null")
final val illegalPairAccess = throw new IllegalArgumentException("Atempted to access a reassign a ")

class Interpreter(prog: Prog = Prog(List.empty, List.empty)) {
  val reader = TerminalReader
  def funcListToMap(funcs: List[Func]): Map[QualifiedFunc, Func] =
    funcs.map {
      func => (func.identifier match
      case qf: QualifiedFunc => qf)
      -> func }.toMap()

  def reset() = {
    heap.clear()
    identTables.clear()
    mutableFuncTable.clear()
  }

  def printLocals() = {
    identTables.headOption match {
    case Some(topTable) if topTable.nonEmpty =>
      topTable.foreach { case (qn, value) =>
        println(s"${qn.t} ${qn.name} = ${itemStringHandler(Some(value))}")
      }
    case _ =>
      println("No variables in the current scope.")
    }
  }

  def printFunctions(): Unit = {
    val allFuncs = mutableFuncTable.keys ++ funcTable.keys
    if (allFuncs.isEmpty) {
      println("No functions defined.")
    } else {
      allFuncs.foreach { qf =>
        val paramStr = qf.paramTypes.mkString(", ")
        println(s"${qf.t} ${qf.funcName}($paramStr)")
      }
    }
  }


  def addFuncsToMutableFuncTable(funcs: List[Func]) = mutableFuncTable.addAll(funcListToMap(funcs))

  val heap: mutable.Map[QualifiedNameContainer, TypeOrPairElemValue] = mutable.Map()
  val identTables: mutable.Stack[mutable.Map[QualifiedName, TypeOrPairElemValue]] = mutable.Stack()
  val mutableFuncTable: mutable.Map[QualifiedFunc, Func] = mutable.Map()
  lazy val funcTable: immutable.Map[QualifiedFunc, Func] = funcListToMap(prog.funcs)
  identTables.push(mutable.Map())

  def stmtsHandler(stmts: List[Stmt]): Option[TypeOrPairElemValue] =
    stmts.collectFirst { case stmt if stmtHandler(stmt).isDefined => stmtHandler(stmt).get }

  def execute(prog: Prog): Unit = try {
    stmtsHandler(prog.main)
  } catch {
    case FailedToEvaluate(message) => println(message)
    case ExitException(code) => (code: @unchecked) match
      case Some(IntLiteral(int)) => println(s"Exited with code: $int")
  }

  def stmtHandler(stmt: Stmt): Option[TypeOrPairElemValue] =
    stmt: @unchecked match {
      case Return(expr) => evaluate(expr)
      case Print(expr) => printHandler(evaluate(expr), PrintType.Print)
      case Println(expr) => printHandler(evaluate(expr), PrintType.PrintLn)
      case WhileDo(condition, stmts) => {
        var ret: Option[TypeOrPairElemValue] = None
        while (ret.isEmpty && evaluate(condition).collect {
          case BoolLiteral(bool: Boolean) => bool
        }.getOrElse(exprCouldntEval))
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
      case Read(lValue: LValue) => readHandler(lValue)

      case Free(expr) => {
        evaluate(expr) match
          case None => exprCouldntEval
          case Some(value) => (value: @unchecked) match
            case StringLiteral(string) => ???
            case arr: ArrayBaseLiteral => arr.elems match
              case None => exprCouldntEval
              case Some(value) => arr.elems = None; None
            case p: PairLiteral => (p.fst, p.snd) match
              case (Some(fst), Some(snd)) => p.fst = None; p.snd = None; None
              case _ => exprCouldntEval
            case NullLiteral => ???
      }
      case Exit(code) => throw new ExitException((evaluate(code): @unchecked) match
        case Some(value) => (value: @unchecked) match
          case IntLiteral(int) => Some(IntLiteral(int % 128))
      )
  }

  def readHandler(lValue: LValue): Option[TypeOrPairElemValue] = {
    (lValue: @unchecked) match
      case qn: QualifiedName =>
        (qn.t: @unchecked) match
          case IntType => reader.readInt() match
            case None => ???
            case Some(int) => reasgnHandler(lValue, IntLiteral(int))
          case CharType => reader.nextChar() match
            case None => ???
            case Some(value) => value match
              case c: Char => reasgnHandler(lValue, CharLiteral(c))

      case ArrayElem(qn: QualifiedName, indexes) => (arrayLiterAccessHandler(qn, indexes): @unchecked) match
        case IntLiteral(int) => reader.readInt() match
            case None => ???
            case Some(int) => arrayLiterAssgnHandler(qn, indexes, Some(IntLiteral(int)))
        case CharLiteral(char) => reader.nextChar() match
            case None => ???
            case Some(value) => value match
              case c: Char => arrayLiterAssgnHandler(qn, indexes, Some(CharLiteral(c)))

      case lValue: (Fst | Snd) => pairAccessHandler(lValue) match
        case None => ???
        case Some(value) => (value: @unchecked) match
          case IntLiteral(int) => reader.readInt() match
            case None => ???
            case Some(int) => reasgnHandler(lValue, IntLiteral(int))

          case CharLiteral(char) => reader.nextChar() match
            case None => ???
            case Some(char) => reasgnHandler(lValue, CharLiteral(char))
  }

  def reasgnHandler(lValue: LValue, rValue: RValue): Option[TypeOrPairElemValue] = {
    val newRValue = rValueHandler(rValue)

    (lValue: @unchecked) match
      case qn: QualifiedName => identTables.top(qn) = newRValue.getOrElse(exprCouldntEval); None
      case ArrayElem(arrayName: QualifiedName, indexes) => arrayLiterAssgnHandler(arrayName, indexes, newRValue)

      case lValue: Fst => {
        val cont = pairReasgnHandler(lValue, newRValue)
        cont match
          case Some(cont) =>
            val current = heap(cont)
            heap(cont) = (current: @unchecked) match
            case PairLiteral(_, snd) => PairLiteral(newRValue, snd)
          case None =>
        None
      }

      case lValue: Snd => {
        val cont = pairReasgnHandler(lValue, newRValue)
        cont match
          case Some(cont) =>
            val current = heap(cont)
            heap(cont) = (current: @unchecked) match
            case PairLiteral(fst, _) => PairLiteral(fst, newRValue)
          case None =>
        None
      }
  }

  def pairReasgnHandler(lValue: LValue, newRValue: Option[TypeOrPairElemValue] = None): Option[QualifiedNameContainer] ={
    (lValue: @unchecked) match
      case qn: QualifiedName => (identTables.top(qn): @unchecked) match
        case cont: QualifiedNameContainer => Some(cont)

      case ArrayElem(arrayName: QualifiedName, indexes) => arrayLiterAssgnHandler(arrayName, indexes, newRValue)

      case Fst(lValue) => pairReasgnHandler(lValue, newRValue) match
        case None => ???
        case Some(value) => value match
          case container: QualifiedNameContainer => Some(container)

      case Snd(lValue) => pairReasgnHandler(lValue, newRValue) match
        case None => ???
        case Some(value) => value match
          case container: QualifiedNameContainer => Some(container)
  }

  def pairAccessHandler(lValue: LValue): Option[TypeOrPairElemValue] = (lValue: @unchecked) match
    case qn: QualifiedName => Some(identTables.top(qn))
    case Fst(lValue) => pairAccessHandler(lValue) match
      case None => ???
      case Some(value) => (value: @unchecked) match
        case container: QualifiedNameContainer => (heap(container): @unchecked) match
          case PairLiteral(fst, snd) => fst
          case NullLiteral => nullAccess

    case Snd(lValue) => pairAccessHandler(lValue) match
      case None => ???
      case Some(value) => (value: @unchecked) match
        case container: QualifiedNameContainer => (heap(container): @unchecked) match
          case PairLiteral(fst, snd) => snd
          case NullLiteral => nullAccess

  def resolveArrayElem(arrayName: (QualifiedName | QualifiedNameContainer), indexes: List[Expr]): (ArrayBaseLiteral, Int) = {
    // Retrieve the array key from the current scope
    val cont: QualifiedNameContainer = arrayName match
      case qn: QualifiedName=> (identTables.top(qn): @unchecked) match
        case cont: QualifiedNameContainer => cont
        case NullLiteral => nullAccess

      case cont: QualifiedNameContainer => cont


    // Retrieve the array from the heap
    (heap.get(cont): @unchecked) match {
      case Some(array: ArrayBaseLiteral) =>
        // Evaluate the index expressions
        val evaluatedIndexes = indexes.map(evaluate(_))

        // Collect valid integer indices
        val resolvedIndexes = evaluatedIndexes.collect {
          case Some(IntLiteral(i)) => i
        }

        // Check if all indices were successfully evaluated
        if (resolvedIndexes.size != evaluatedIndexes.size) {
          throw new NoSuchElementException("Some index expressions could not be evaluated")
        }
        // Traverse the array using the indices
        resolvedIndexes.init.foldLeft(Option(array): Option[ArrayBaseLiteral]) {
          case (Some(ArrayBaseLiteral(Some(arr))), index) =>
            arr.lift(index.toInt) match {
              case None => throw new NoSuchElementException(s"Index $index is out of bounds")
              case Some(value) => value match {
                case arr: ArrayBaseLiteral => Some(arr)
                case cont: QualifiedNameContainer => (heap(cont): @unchecked) match
                  case arr: ArrayBaseLiteral => Some(arr)
                  case PairLiteral(fst, snd) => ???
                  case NullLiteral => ???

                case PairLiteral(_, _) => throw new UnsupportedOperationException("Cannot index into a pair")
                case NullLiteral => throw new NoSuchElementException("Array element is null")
                case _ => throw new UnsupportedOperationException(s"Cannot index into ${value.getClass.getSimpleName}")
              }
            }
          case _ => None
        } match {
          case Some(arr @ ArrayBaseLiteral(Some(_))) =>
            val lastIndex = resolvedIndexes.last
            (arr, lastIndex.toInt)
          case _ => throw new NoSuchElementException("Array has been freed or not found")
        }
    }
  }

  def arrayLiterAssgnHandler(arrayName: QualifiedName, indexes: List[Expr], newRValue: Option[TypeOrPairElemValue]) = {
    val (arrayBase, lastIndex) = resolveArrayElem(arrayName, indexes)
    arrayBase.elems match
      case Some(arr) => arr(lastIndex) = newRValue.getOrElse(exprCouldntEval)
      case None => throw new NoSuchElementException("Array has been freed")
    None
  }

  def arrayLiterAccessHandler(arrayName: QualifiedName, indexes: List[Expr]): TypeOrPairElemValue = {
    val (arrayBase, lastIndex) = resolveArrayElem(arrayName, indexes)
    arrayBase.elems match
      case Some(arr) => arr(lastIndex)
      case None => throw new NoSuchElementException("Array has been freed")
  }

  def rValueHandler(rValue: RValue): Option[TypeOrPairElemValue] = (rValue: @unchecked) match
    case expr: Expr => evaluate(expr)
    case Call(qf: QualifiedFunc, exprs: List[Expr]) => callHandler(qf, exprs)
    case fst: Fst => pairAccessHandler(fst) match
      case None => ???
      case Some(value) => (value) match
        case e: Expr => evaluate(e)
        case qn: QualifiedNameContainer => Some(qn)
        case arr: ArrayBaseLiteral => Some(arr)
        case pair: PairLiteral => Some(pair)

    case snd: Snd => pairAccessHandler(snd) match
      case None => ???
      case Some(value) => (value: @unchecked) match
        case e: Expr => evaluate(e)
        case qn: QualifiedNameContainer => Some(qn)
        case arr: ArrayBaseLiteral => Some(arr)
        case pair: PairLiteral => Some(pair)

    case ArrayLiter(exprs: List[Expr]) =>  exprs.traverse(evaluate).map(elems => ArrayBaseLiteral(Some(elems.toArray)))
    case NewPair(e1: Expr, e2: Expr) => Some(PairLiteral(evaluate(e1), evaluate(e2)))

  def assgnHandler: Assgn => Option[TypeOrPairElemValue] = {
    case Assgn(t, identifier, rValue) => {
      val result = rValueHandler(rValue).getOrElse(exprCouldntEval)
      val qn = identifier match
        case qn: QualifiedName => qn

      qn.t match
        case t: (PairType | ArrayType) => {
          heap.addOne(QualifiedNameContainer(qn), result)
          identTables.top(qn) = QualifiedNameContainer(qn)
        }
        case _ => identTables.top(qn) = result
      None
    }
  }

  def callHandler(qf: QualifiedFunc, exprs: List[Expr]): Option[TypeOrPairElemValue] = {
    val func = mutableFuncTable.get(qf) match
      case None => funcTable.get(qf) match
        case None => println(s"Function `${qf.funcName} does not exist`"); return None
        case Some(value) => value
      case Some(value) => value

    val newIdentTable = (func.params.reverse.zip(exprs).map {
      (param, expr) => ((param.identifier: @unchecked) match
        case qn: QualifiedName => qn)
      -> evaluate(expr).getOrElse(exprCouldntEval)
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
    case qn: QualifiedName => identTables.top.get(qn) match
      case Some(value) => value match
        case PairLiteral(_, _) => Some(QualifiedNameContainer(qn))
        case v => Some(v)
      case None => println(s"`${qn.name}` not found in current scope"); None

    case ArrayElem(arrayName: Ident, indexes: List[Expr]) =>
      arrayName match
        case arrayName: QualifiedName => Some(arrayLiterAccessHandler(arrayName, indexes))
    case NullLiteral => Some(NullLiteral)

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
        case (Some(NullLiteral), Some(NullLiteral)) => Some(BoolLiteral(true))
        case (Some(p1: PairLiteral), Some(NullLiteral)) => Some(BoolLiteral(false))
        case (Some(NullLiteral), Some(p2: PairLiteral)) => Some(BoolLiteral(false))

      case NotEq(l, r) => (binEvaluator(Eq(l, r).asInstanceOf[BinaryOperator]).getOrElse(exprCouldntEval): @unchecked) match
        case BoolLiteral(bool) => Some(BoolLiteral(!bool))

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

  def itemStringHandler(item: Option[TypeOrPairElemValue]): String = (item: @unchecked) match
    case Some(IntLiteral(int)) => int.toString()
    case Some(BoolLiteral(bool)) => bool.toString()
    case Some(StringLiteral(string)) => string
    case Some(CharLiteral(char)) => char.toString()
    case Some(ArrayBaseLiteral(Some(elems))) =>
      elems.collect { case (CharLiteral(c)) => c }.mkString match {
        case "" => s"0x${System.identityHashCode(elems).toHexString}" // If no CharLiterals are found
        case str => str
      }
    case Some(pair @ PairLiteral(_, _)) => s"0x${System.identityHashCode(pair).toHexString}"
    case Some(cont: QualifiedNameContainer) => itemStringHandler(Some(heap(cont)))
    case Some(NullLiteral) => "(nil)"
    case None => ""
}