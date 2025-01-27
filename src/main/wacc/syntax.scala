package wacc
import scala.language.implicitConversions
import parsley.Parsley._
import parsley.Parsley
import parsley.generic._
import parsley.ap._


case class Prog(funcs: List[Func], main: List[Stmt])
case class Func(t: Type, ident: Ident, params: List[Param], stmt: Stmt)

sealed trait RValue
sealed trait LValue
sealed trait Expr extends RValue
sealed trait Stmt

case class Ident(identifier: String) extends Expr, LValue, RValue
case class PairElem(lValue: LValue, fstorsnd: String) extends LValue, RValue
case class ArrayElem(arrayName: Ident, index: List[Expr]) extends Expr, LValue

case class ArrayLiter(elems: List[Expr]) extends RValue
case class NewPair(fst: Expr, snd: Expr) extends RValue
case class FuncCall(ident: Ident, params: List[Expr]) extends RValue

//Literals
case class IntLiteral(int: BigInt) extends Expr
case class BoolLiteral(bool: Boolean) extends Expr
case class StringLiteral(string: String) extends Expr
case class CharLiteral(char: Char) extends Expr
case object NullLiteral extends Expr


case class Param(t: Type, ident: Ident)


trait BinaryBridge extends ParserBridge2[Expr, Expr, Expr]
trait UnaryBridge extends ParserBridge1[Expr,Expr]

case class Neg(x: Expr) extends Expr
case class Not(x: Expr) extends Expr
case class Len(x: Expr) extends Expr
case class Chr(x: Expr) extends Expr
case class Ord(x: Expr) extends Expr
object Neg extends UnaryBridge
object Not extends UnaryBridge
object Len extends UnaryBridge
object Chr extends UnaryBridge
object Ord extends UnaryBridge

case class Mul(l: Expr, r: Expr) extends Expr
case class Div(l: Expr, r: Expr) extends Expr
case class Mod(l: Expr, r: Expr) extends Expr
case class Add(l: Expr, r: Expr) extends Expr
case class Sub(l: Expr, r: Expr) extends Expr
case class Less(l: Expr, r: Expr) extends Expr
case class LessE(l: Expr, r: Expr) extends Expr
case class Greater(l: Expr, r: Expr) extends Expr
case class GreaterE(l: Expr, r: Expr) extends Expr
case class Eq(l: Expr, r: Expr) extends Expr
case class NotEq(l: Expr, r: Expr) extends Expr
case class And(l: Expr, r: Expr) extends Expr
case class Or(l: Expr, r: Expr) extends Expr
object Mul extends BinaryBridge
object Div extends BinaryBridge
object Mod extends BinaryBridge
object Add extends BinaryBridge
object Sub extends BinaryBridge
object Less extends BinaryBridge
object LessE extends BinaryBridge
object Greater extends BinaryBridge
object GreaterE extends BinaryBridge
object Eq extends BinaryBridge
object NotEq extends BinaryBridge
object And extends BinaryBridge
object Or extends BinaryBridge





sealed trait Type 
sealed trait PairElemType
sealed trait BaseType extends Type, PairElemType 

case class PairType(t1: PairElemType, t2: PairElemType) extends Type
case class ArrayType(t: Type) extends Type, PairElemType
case object Pair extends PairElemType

case object IntType extends BaseType, Expr
case object BoolType extends BaseType
case object StringType extends BaseType
case object CharType extends BaseType
