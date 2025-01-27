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

    //Ambiguity. When faced with ident is it just an ident or an array-elem.
    private lazy val expr: Parsley[Expr] =
    precedence(
        //We should abstract the atoms to be parsed seperately ( string, char, int, bool,)
        stringliteral.map(StringAtom.apply(_)),
        charliteral.map(CharAtom.apply(_)),
        intliteral.map(IntAtom.apply(_)),
        boolLiteral.map(BoolAtom.apply(_)),
        // nullliteral, // What do we do with this?
        atomic(ident ~> lSquare ~> expr <~ rSquare), // Since arrays dont exist, just taking the index for now
        ident.map(StringAtom.apply(_)), //We need to store idents and their values, for now just treating as a string
        lParen ~> expr <~ rParen,
    )( unaryOps, mulDivOps, addSubOps, comparisonOps, equalOps, boolOps)
}
