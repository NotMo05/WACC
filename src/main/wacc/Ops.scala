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
    ("!"    as (expr => Not(expr))),
    ("-"    as (expr => Neg(expr))),
    ("len"  as (expr => Len(expr))), // Len takes in a list, doesnt exist yet so made it work for strings for testing
    ("ord"  as (expr => Ord(expr))),
    ("char" as (expr => Chr(expr)))
  )

val mulDivOps: Ops[Expr, Expr] = Ops(InfixL)(
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

val boolOps: Ops[Expr, Expr] = Ops(InfixR)(
    ("&&" as ((l, r) => And(l,r))),
    ("||" as ((l, r) => Or(l,r)))
  )
