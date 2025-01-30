package wacc

import lexer.implicits.implicitSymbol
import parsley.combinator.{sepBy1}
import parsley.expr.{precedence}
import parsley.{Parsley, Result}
import parsley.Parsley.{atomic, some, pure, notFollowedBy, many}
import wacc.lexer._

object parser {
  def parse(input: String): Result[String, Prog] = parser.parse(input)
  private val parser = fully(program)

  // def parse(input: String): Result[String, Stmt] = parser.parse(input)
  // private val parser = fully(stmt)

  // def parse(input: String): Result[String, Expr] = parser.parse(input)
  // private val parser = fully(expr)
  
  private lazy val program = Prog("begin" ~> many(atomic(func)), stmts <~ "end")

  private lazy val func = Func(typeParser, ident, "(" ~> (atomic(paramList) | (pure(List.empty[Param]))) <~ ")", "is" ~> stmts <~ "end")

  private lazy val param = atomic(Param(typeParser, ident <~ notFollowedBy("="))) 

  private lazy val paramList = sepBy1(param, ",")

  lazy val pairType = PairType("pair" ~> "(" ~> pairElemType, "," ~> pairElemType <~ ")")

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
  private lazy val pairElem = PairElem(fstOrSnd, lValue)
  
  private lazy val lValue: Parsley[LValue] =
    atomic(arrayElem)
    | ident
    | pairElem

  private lazy val call = Call("call" ~> ident, "(" ~> (atomic(argList) | (pure(List.empty[Expr]))) <~ ")")

  private lazy val argList: Parsley[List[Expr]] = sepBy1(expr, ",")
  private lazy val arrayLiteral =  ArrayLiter("[" ~> (atomic(argList) | (pure(List.empty[Expr]))) <~ "]")
  private lazy val newPair = NewPair("newpair" ~> "(" ~> expr <~ ",", expr <~ ")" )
  private lazy val rValue: Parsley[RValue] =
      expr
    | arrayLiteral
    | newPair
    | pairElem
    | call

  private lazy val expr: Parsley[Expr] =
    precedence(
      atoms,
      atomic(arrayElem),
      ident,
      "(" ~> expr <~ ")"
  )(unaryOps, mulDivModOps, addSubOps, comparisonOps, equalOps, logicOps)

  private lazy val beginBlock = Scope("begin" ~> stmts <~ "end")

  private lazy val stmt: Parsley[Stmt] =
    atomic(assgn)
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
    | beginBlock

}
