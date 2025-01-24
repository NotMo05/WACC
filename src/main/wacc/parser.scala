package wacc

import parsley.{Parsley, Result}
import parsley.Parsley._
import lexer.implicits.implicitSymbol

import wacc._
import lexer._
import parsley.expr._


object parser {

    def parse(input: String): Result[String, Expr] = parser.parse(input)
    private val parser = fully(expr)

    private lazy val expr: Parsley[Expr] =
    precedence(
        intliteral.map(IntAtom.apply(_)),
        boolLiteral.map(BoolAtom.apply(_)),
        lParen ~> expr <~ rParen
    )(
    // Unary operations
    Ops(Prefix)(
      ("!" as (expr => BoolAtom(!expr.asInstanceOf[BoolAtom].bool))),
      ("-" as (expr => IntAtom(-expr.asInstanceOf[IntAtom].int))),
      // ("len" as (expr => IntAtom(-expr.asInstanceOf[IntAtom].int))),
      // ("ord" as (expr => IntAtom(-expr.asInstanceOf[IntAtom].int))),
      // ("char" as (expr => IntAtom(-expr.asInstanceOf[IntAtom].int)))
    ),

    // Arithmetic operations
    Ops(InfixL)(
      ("*" as ((l, r) => IntAtom(
        l.asInstanceOf[IntAtom].int * r.asInstanceOf[IntAtom].int))),
      ("/" as ((l, r) => IntAtom(
        l.asInstanceOf[IntAtom].int / r.asInstanceOf[IntAtom].int))),
      ("%" as ((l, r) => IntAtom(
        l.asInstanceOf[IntAtom].int % r.asInstanceOf[IntAtom].int)))
    ),
    Ops(InfixL)(
      ("+" as ((l, r) => IntAtom(
        l.asInstanceOf[IntAtom].int + r.asInstanceOf[IntAtom].int))),
      ("-" as ((l, r) => IntAtom(
        l.asInstanceOf[IntAtom].int - r.asInstanceOf[IntAtom].int)))
    ),

    // Comparison and logical operations
    Ops(InfixN)(
      ("<" as ((l, r) => BoolAtom(
        l.asInstanceOf[IntAtom].int < r.asInstanceOf[IntAtom].int))),
      ("<=" as ((l, r) => BoolAtom(
        l.asInstanceOf[IntAtom].int <= r.asInstanceOf[IntAtom].int))),
      (">" as ((l, r) => BoolAtom(
        l.asInstanceOf[IntAtom].int > r.asInstanceOf[IntAtom].int))),
      (">=" as ((l, r) => BoolAtom(
        l.asInstanceOf[IntAtom].int >= r.asInstanceOf[IntAtom].int)))
    ),
    Ops(InfixN)(
      ("==" as ((l, r) => BoolAtom(l == r))),
      ("!=" as ((l, r) => BoolAtom(l != r))) 
    ),
    Ops(InfixR)(
      ("&&" as ((l, r) => BoolAtom(
        l.asInstanceOf[BoolAtom].bool && r.asInstanceOf[BoolAtom].bool))),
      ("||" as ((l, r) => BoolAtom(
        l.asInstanceOf[BoolAtom].bool || r.asInstanceOf[BoolAtom].bool)))
    )
  )

// Atoms remaining to be added to expr / tokenised
//   ⟨atom⟩ ::= ⟨char-liter⟩
// | ⟨str-liter⟩
// | ⟨pair-liter⟩
// | ⟨ident⟩
// | ⟨array-elem⟩

}
