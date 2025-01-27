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

    )(
    // Basic rundown of how this currently works, we take an expr, exprs can be any of these atoms ( see Syntax.scala ) and more.
    // We downcast ( bad but only for now ) via asInstanceOf, do what we need now that we "know" the type and then turn back into an expr via ___Atom() to be returned.
    // Later on ( or if someone can be bothered now )we should probably use match cases on types instead

    unaryOps, mulDivOps, addSubOps, comparisonOps, equalOps, boolOps)
}
