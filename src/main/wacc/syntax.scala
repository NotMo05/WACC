package wacc

import scala.language.implicitConversions
import parsley.generic.{ParserBridge1, ParserBridge2, ParserBridge3, ParserBridge4}

sealed trait Stmt
sealed trait RValue
sealed trait LValue
sealed trait Expr extends RValue

sealed trait Pos
case object Fst extends Pos
case object Snd extends Pos

// Expressions

case class IntLiteral(int: BigInt) extends Expr
case class BoolLiteral(bool: Boolean) extends Expr
case class StringLiteral(string: String) extends Expr
case class CharLiteral(char: Char) extends Expr
case class Ident(identifier: String) extends Expr, LValue, RValue
case class ArrayElem(arrayName: Ident, index: List[Expr]) extends Expr, LValue

object IntLiteral extends ParserBridge1[BigInt, IntLiteral]
object BoolLiteral extends ParserBridge1[Boolean, BoolLiteral]
object StringLiteral extends ParserBridge1[String, StringLiteral]
object CharLiteral extends ParserBridge1[Char, CharLiteral]
object Ident extends ParserBridge1[String, Ident]
object ArrayElem extends ParserBridge2[Ident, List[Expr], ArrayElem]

case object NullLiteral extends Expr

// Statements

case class Prog(funcs: List[Func], main: List[Stmt])
case class Func(t: Type, ident: Ident, params: List[Param], stmt: Stmt)
case class Call(ident: Ident, args: List[Expr]) extends RValue
case class Param(t: Type, ident: Ident)
case class PairElem(fstOrSnd: Pos, lValue: LValue) extends LValue, RValue
case class ArrayLiter(elems: List[Expr]) extends RValue
case class NewPair(fst: Expr, snd: Expr) extends RValue

object Prog extends ParserBridge2[List[Func], List[Stmt], Prog]
object Func extends ParserBridge4[Type, Ident, List[Param], Stmt, Func]
object Call extends ParserBridge2[Ident, List[Expr], RValue]
object Param extends ParserBridge2[Type, Ident, Param]
object PairElem extends ParserBridge2[Pos, LValue, PairElem]
object ArrayLiter extends ParserBridge1[List[Expr], RValue]
object NewPair extends ParserBridge2[Expr, Expr, NewPair]

case object Skip extends Stmt
case class Read(lValue: LValue) extends Stmt
case class Free(expr: Expr) extends Stmt
case class Return(expr: Expr) extends Stmt
case class Exit(expr: Expr) extends Stmt
case class Print(expr: Expr) extends Stmt
case class Println(expr: Expr) extends Stmt
case class WhileDo(condition: Expr, stmts: List[Stmt]) extends Stmt
case class IfElse(condition: Expr, thenStmts: List[Stmt], elseStmts: List[Stmt]) extends Stmt
case class Assgn(t: Type, identifier: Ident, rValue: RValue) extends Stmt
case class ReAssgn(lValue: LValue, rValue: RValue) extends Stmt

object Read extends ParserBridge1[LValue, Stmt]
object Free extends ParserBridge1[Expr, Stmt]
object Return extends ParserBridge1[Expr, Stmt]
object Exit extends ParserBridge1[Expr, Stmt]
object Print extends ParserBridge1[Expr, Stmt]
object Println extends ParserBridge1[Expr, Stmt]
object WhileDo extends ParserBridge2[Expr, List[Stmt], Stmt]
object IfElse extends ParserBridge3[Expr, List[Stmt], List[Stmt], Stmt]
object Assgn extends ParserBridge3[Type, Ident, RValue, Stmt]
object ReAssgn extends ParserBridge2[LValue, RValue, Stmt]

//Types

sealed trait Type extends Expr
sealed trait PairElemType
sealed trait BaseType extends Type, PairElemType

case class PairType(t1: PairElemType, t2: PairElemType) extends Type
case class ArrayType(t: Type) extends Type, PairElemType
case object Pair extends PairElemType

case object IntType extends BaseType
case object BoolType extends BaseType
case object StringType extends BaseType
case object CharType extends BaseType

// Operators

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