package wacc
import parsley.{Parsley, Result}
import parsley.Parsley._
import lexer.implicits.implicitSymbol
// If anyone knows how to import from syntax.scala, then we can get rid of importing the whole package
import wacc._
import lexer._
import parsley.expr._


object parser {

    def parse(input: String): Result[String, Expr] = parser.parse(input)
    private val parser = fully(expr)

    private lazy val expr: Parsley[Expr] =
    precedence(
        // Once we have the remaining atoms done ( see bottom ) we should abstract the atoms to be parsed seperately
        stringliteral.map(StringAtom.apply(_)),
        charliteral.map(CharAtom.apply(_)),
        intliteral.map(IntAtom.apply(_)),
        boolLiteral.map(BoolAtom.apply(_)),
        lParen ~> expr <~ rParen
    )(

    // Basic rundown of how this currently works, we take an expr, exprs can be any of these atoms ( see Syntax.scala ) and more.
    // We downcast ( bad but only for now ) via asInstanceOf, do what we need now that we "know" the type and then turn back into an expr via ___Atom() to be returned.
    // Safer way would probably be to have each of these functions do a match case thing where we match types but thats a lot of writing so I'll
    // leave that to someone else, goodbye

    // Unary operations
    Ops(Prefix)(
      ("!" as (_ => BoolAtom(!_.asInstanceOf[BoolAtom].bool))),
      ("-" as (_ => IntAtom(-_.asInstanceOf[IntAtom].int))),
      ("len" as (_ => IntAtom(_.asInstanceOf[StringAtom].string.length))), // Len takes in a list, doesnt exist yet so made it work for strings for testing
      ("ord" as (_ => IntAtom(_.asInstanceOf[CharAtom].char.toInt))),
      ("char" as (_ => CharAtom(_.asInstanceOf[IntAtom].int.toChar)))
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
//   ⟨atom⟩ ::= | ⟨pair-liter⟩
// | ⟨ident⟩
// | ⟨array-elem⟩

}
