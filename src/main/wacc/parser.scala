package wacc

import lexer.implicits.implicitSymbol
import parsley.combinator.{sepBy1, optionalAs}
import parsley.expr.{precedence}
import parsley.{Parsley, Result}
import parsley.Parsley.{some}
import wacc.lexer._

object parser {
  def parse(input: String): Result[String, Expr] = parser.parse(input)
  private val parser = fully(expr)

  private lazy val atoms =
    stringLiteral
    | charLiteral
    | intLiteral
    | boolLiteral
    | nullLiteral.map(_ => NullLiteral)


  val stmts: Parsley[List[Stmt]] = sepBy1(stmt, ";")

  private lazy val assgn = Assgn(typeParser, ident, "=" ~> rValue)
  private lazy val reassgn = ReAssgn(lValue, "=" ~> rValue)
  private lazy val readStmt = Read("read" ~> lValue)
  private lazy val freeStmt = Free("free" ~> expr)
  private lazy val returnStmt = Return("return" ~> expr)
  private lazy val exitStmt = Exit("exit" ~> expr)
  private lazy val printlnStmt = Println("println" ~> expr)
  private lazy val printStmt = Print("print" ~> expr)
  private lazy val whileStmt = WhileDo("while" ~> expr, "do" ~> stmts <~ "done" )
  private lazy val ifStmt = IfElse("if" ~> expr, "then" ~> stmts, "else" ~> stmts <~ "fi")

  private lazy val arrayElem = ArrayElem(ident, some("[" ~> expr <~ "]"))
  private lazy val pairElem = PairElem(lValue, posParser)

  private lazy val lValue: Parsley[LValue] =
      arrayElem
    | ident
    | pairElem

  private lazy val call = Call("call" ~> ident, "(" ~> optionalAs(argList, List.empty[Expr]) <~ ")")
  private lazy val argList: Parsley[List[Expr]] = sepBy1(expr, ",")
  private lazy val arrayLiteral = ArrayLiter("[" ~> optionalAs(argList, List.empty[Expr]) <~ "]")
  private lazy val newPair = NewPair("(" ~> expr <~ ",", expr <~ ")" )
  private lazy val rValue: Parsley[RValue] =
      expr
    | arrayLiteral
    | newPair
    | pairElem
    | call

  private lazy val expr: Parsley[Expr] =
    precedence(
      atoms,
      arrayElem,
      ident,
      "(" ~> expr <~ ")",
    )(unaryOps, mulDivModOps, addSubOps, comparisonOps, equalOps, logicOps)

  private lazy val stmt: Parsley[Stmt] =
      assgn
    | reassgn
    | readStmt
    | freeStmt
    | returnStmt
    | exitStmt
    | printlnStmt
    | printStmt
    | whileStmt
    | ifStmt
    | skipStmt
    | "begin" ~> stmt <~ "end"

}
