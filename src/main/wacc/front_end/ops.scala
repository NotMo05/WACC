package wacc.front_end

import lexer.implicits.implicitSymbol
import parsley.expr.{Ops, Prefix, InfixL, InfixN, InfixR}
import parsley.combinator.{ifS}
import parsley.Parsley.{empty, atomic, pure}
import wacc.front_end.lexer.integer

val unaryOps: Ops[Expr, Expr] = Ops(Prefix)(
  ("!"   as (expr => Not(expr))),
  (atomic(ifS(atomic(integer) ~> pure(true)
  | pure(false), empty, "-" as (expr => Neg(expr))))),
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

val andOp: Ops[Expr, Expr] = Ops(InfixR)(
  ("&&" as ((l, r) => And(l,r)))
)

val orOp: Ops[Expr, Expr] = Ops(InfixR)(
  ("||" as ((l, r) => Or(l,r)))
)