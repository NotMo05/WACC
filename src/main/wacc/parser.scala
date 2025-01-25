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
        stringliteral.map(StringAtom.apply(_)),
        charliteral.map(CharAtom.apply(_)),
        intliteral.map(IntAtom.apply(_)),
        boolLiteral.map(BoolAtom.apply(_)),
        lParen ~> expr <~ rParen
    )(
    // Unary operations
    Ops(Prefix)(
      ("!" as (expr => BoolAtom(!expr.asInstanceOf[BoolAtom].bool))),
      ("-" as (expr => IntAtom(-expr.asInstanceOf[IntAtom].int))),
      ("len" as (expr => IntAtom(expr.asInstanceOf[StringAtom].string.length))), // Len takes in a list, doesnt exist yet so made it work for strings for testing
      ("ord" as (expr => IntAtom(expr.asInstanceOf[CharAtom].char.toInt))),
      ("char" as (expr => CharAtom(expr.asInstanceOf[IntAtom].int.toChar)))
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
