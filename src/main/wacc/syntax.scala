package wacc
import scala.language.implicitConversions
import parsley.Parsley._
import parsley.Parsley


case class Prog(funcs: List[Func], main: List[Stmt])
case class Func(t: Type, ident: Ident, params: List[Param], stmt: Stmt)

sealed trait RValue
sealed trait LValue
sealed trait Expr extends RValue
// object Expr {
//   // Implicit conversion from Int to Expr
//   implicit def intToExpr(x: Parsley[BigInt]): Parsley[Expr] = pure(IntAtom(x))
// }
sealed trait Stmt

case class Ident(identifier: String) extends Expr, LValue, RValue
case class PairElem(lValue: LValue, fstorsnd: String) extends LValue, RValue
case class ArrayElem(arrayName: Ident, index: Expr) extends LValue

case class ArrayLiter(elems: List[Expr]) extends RValue
case class NewPair(fst: Expr, snd: Expr) extends RValue
case class FuncCall(ident: Ident, params: List[Expr]) extends RValue

case class IntAtom(int: BigInt) extends Expr
case class BoolAtom(bool: Boolean) extends Expr


case class Param(t: Type, ident: Ident)


// trait BinaryBridge extends ParserBridgeInfo2[Expr, Expr, Expr]
// object Mul extends BinaryBridge
// case class Mul(lhs: Expr, rhs: Expr)(val info: Info) extends Expr derives ParserBridgeInfo2






sealed trait Type 
sealed trait PairElemType
sealed trait BaseType extends Type, PairElemType 

case class PairType(t1: PairElemType, t2: PairElemType) extends Type
case object Pair extends PairElemType
case class ArrayType(t: Type) extends Type, PairElemType

case object IntType extends BaseType
case object BoolType extends BaseType
case object StringType extends BaseType
case object CharType extends BaseType
