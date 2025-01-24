package wacc

import parsley.{Parsley, Result}
import parsley.expr.chain

import lexer.implicits.implicitSymbol
import lexer.{intliteral, fully,ident}
import parsley.expr.precedence
import parsley.expr._

object parser {
    def parse(input: String): Result[String, BigInt] = parser.parse(input)
    private val parser = fully(expr)

    private lazy val expr: Parsley[BigInt] =
        precedence(intliteral, ident as BigInt(10), "(" ~> expr <~ ")")(
            Ops(Prefix)("-" as ( - _)),
            Ops(InfixL)(("*" as (_ * _)), ("%" as (_ % _)),("/" as (_ / _))),
            Ops(InfixL)(("+" as (_ + _)), ("-" as (_ - _))),
            
            // These are for bools
            //Ops(InfixN)(("<" as (_ < _)), ("<=" as (_ <= _)),(">" as (_ > _),  (">=" as (_ >= _)))),
            //Ops(InfixN)(("==" as (_ == _)), ("!=" as (_ != _))),
            //Ops(InfixR)("&&" as (_ & _)),
            //Ops(InfixR)("||" as ( | _)),
        )
}
