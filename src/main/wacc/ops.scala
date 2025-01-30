package wacc

import lexer.implicits.implicitSymbol
import parsley.expr.{Ops, Prefix, InfixL, InfixN, InfixR}

def handleNegation(expr: Expr): Expr = expr match {
  case IntLiteral(value) => IntLiteral(-value)
  case Neg(x)            => x
  case other             => Neg(other)
}

val unaryOps: Ops[Expr, Expr] = Ops(Prefix)(
  ("!"   as (expr => Not(expr))),
  ("-"   as (expr => handleNegation(expr))),
  ("len" as (expr => Len(expr))),
  ("ord" as (expr => Ord(expr))),
  ("chr" as (expr => Chr(expr)))
)

val mulDivModOps: Ops[Expr, Expr] = Ops(InfixL)(
  ("*" as ((l, r) => Mul(l,r))),
  ("/" as ((l, r) => Div(l,r))),
  ("%" as ((l, r) => Mod(l,r)))
)

val addSubOps: Ops[Expr, Expr] = Ops(InfixL)(
  ("+" as ((l, r) => Add(l,r))),
  ("-" as ((l, r) => Sub(l,r)))
)

val comparisonOps: Ops[Expr, Expr] = Ops(InfixN)(
  ("<"  as ((l, r) => Less(l,r))),
  ("<=" as ((l, r) => LessE(l,r))),
  (">"  as ((l, r) => Greater(l,r))),
  (">=" as ((l, r) => GreaterE(l,r)))
)

val equalOps:Ops[Expr, Expr] = Ops(InfixL)(
  ("==" as ((l, r) => Eq(l,r))),
  ("!=" as ((l, r) => NotEq(l,r)))
)

val logicOps: Ops[Expr, Expr] = Ops(InfixR)(
  ("&&" as ((l, r) => And(l,r))),
  ("||" as ((l, r) => Or(l,r)))
)