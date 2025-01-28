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
