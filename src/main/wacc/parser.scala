package wacc

import lexer.implicits.implicitSymbol
import parsley.expr.{precedence}
import parsley.{Parsley, Result}
import parsley.Parsley.{atomic, some}
import wacc.lexer._

object parser {
    def parse(input: String): Result[String, Expr] = parser.parse(input)
    private val parser = fully(expr)

    private lazy val atoms = 
          stringLiteral.map(StringLiteral.apply(_)) 
        | charLiteral.map(CharLiteral.apply(_))
        | intLiteral.map(IntLiteral.apply(_))
        | boolLiteral.map(BoolLiteral.apply(_))
        | nullLiteral.map(_ => NullLiteral)

    private lazy val expr: Parsley[Expr] =
        precedence(
            atoms,
            atomic(ident <~> some("[" ~> expr <~ "]")).map((ident, expr) => ArrayElem(Ident(ident), expr)),
            ident.map(Ident.apply(_)),
            "(" ~> expr <~ ")",
      )(unaryOps, mulDivOps, addSubOps, comparisonOps, equalOps, boolOps)
}
