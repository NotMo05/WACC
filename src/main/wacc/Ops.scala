package wacc
import parsley.{Parsley, Result}
import parsley.Parsley._
import lexer.implicits.implicitSymbol
// If anyone knows how to import from syntax.scala, then we can get rid of importing the whole package
// and if anyone can be bothered to go through the imports and only import whats needed we can stop getting the warnings
import wacc._
import lexer._
import parsley.expr._
import parsley.token.errors._

val unaryOps: Ops[Expr, Expr] = Ops(Prefix)(
    ("!" as (expr => BoolAtom(!expr.asInstanceOf[BoolAtom].bool))),
    ("-" as (expr => IntAtom(-expr.asInstanceOf[IntAtom].int))),
    ("len" as (expr => IntAtom(expr.asInstanceOf[StringAtom].string.length))), // Len takes in a list, doesnt exist yet so made it work for strings for testing
    ("ord" as (expr => IntAtom(expr.asInstanceOf[CharAtom].char.toInt))),
    ("char" as (expr => CharAtom(expr.asInstanceOf[IntAtom].int.toChar)))
  )

val arithmeticOps: Ops[Expr, Expr] = Ops(InfixL)(
    ("*" as ((l, r) => IntAtom(
      l.asInstanceOf[IntAtom].int * r.asInstanceOf[IntAtom].int))),
    ("/" as ((l, r) => IntAtom(
      l.asInstanceOf[IntAtom].int / r.asInstanceOf[IntAtom].int))),
    ("%" as ((l, r) => IntAtom(
      l.asInstanceOf[IntAtom].int % r.asInstanceOf[IntAtom].int)))
  )

val comparisonOps: Ops[Expr, Expr] = Ops(InfixN)(
    ("<" as ((l, r) => BoolAtom(
      l.asInstanceOf[IntAtom].int < r.asInstanceOf[IntAtom].int))),
    ("<=" as ((l, r) => BoolAtom(
      l.asInstanceOf[IntAtom].int <= r.asInstanceOf[IntAtom].int))),
    (">" as ((l, r) => BoolAtom(
      l.asInstanceOf[IntAtom].int > r.asInstanceOf[IntAtom].int))),
    (">=" as ((l, r) => BoolAtom(
      l.asInstanceOf[IntAtom].int >= r.asInstanceOf[IntAtom].int))),
    ("==" as ((l, r) => BoolAtom(l == r))),
    ("!=" as ((l, r) => BoolAtom(l != r)))
  )

val boolOps: Ops[Expr, Expr] = Ops(InfixR)(
    ("&&" as ((l, r) => BoolAtom(
      l.asInstanceOf[BoolAtom].bool && r.asInstanceOf[BoolAtom].bool))),
    ("||" as ((l, r) => BoolAtom(
      l.asInstanceOf[BoolAtom].bool || r.asInstanceOf[BoolAtom].bool)))
  )