package wacc.front_end

import lexer.implicits.implicitSymbol
import parsley.combinator.{sepBy1, countSome}
import parsley.expr.{precedence}
import parsley.{Parsley, Result}
import parsley.Parsley.{atomic, some, pure, notFollowedBy, many}
import wacc.front_end.lexer._

object parser {
  def parse(input: String): Result[String, Prog] = parser.parse(input)
  private val parser = fully(program)

  private lazy val program = Prog("begin" ~> many(atomic(func)), stmts <~ "end")
  private lazy val func = Func(
  typeParser,
    ident,
    "(" ~> onceOrEmptyList(paramList) <~ ")",
    "is" ~> stmts.filter(returns(_)) <~ "end"
  )

  private lazy val param = (Param(typeParser, ident <~ notFollowedBy("=")))
  private lazy val paramList = sepBy1(param, ",")
  private lazy val pairType = PairType("pair" ~> "(" ~> pairElemType, "," ~> pairElemType <~ ")")

  private lazy val atoms =
    "(" ~> expr <~ ")"
    | stringLiteral
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
  private lazy val pairElem = Fst("fst" ~> lValue) | Snd("snd" ~> lValue)
  private lazy val importStmt = Import("import" ~> stringLiteral) // import statement matching

  private lazy val typeParser = atomic(arrayType) | interimTypes

  private lazy val baseType =
    ("int" as IntType)
    | ("char" as CharType)
    | ("bool" as BoolType)
    | ("string" as StringType)

  private lazy val interimTypes: Parsley[Type] =
    baseType
    | pairType

  private lazy val pairElemType =
    (("pair") as Pair)
    | atomic(arrayType)
    | baseType

  private lazy val arrayType = ArrayType(interimTypes, countSome("[]"))

  private lazy val lValue: Parsley[LValue] =
    atomic(arrayElem)
    | ident
    | pairElem

  private lazy val call = Call("call" ~> ident, "(" ~> onceOrEmptyList(argList) <~ ")")

  private lazy val argList: Parsley[List[Expr]] = sepBy1(expr, ",")
  private lazy val arrayLiteral =  ArrayLiter("[" ~> onceOrEmptyList(argList) <~ "]")
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
  )(unaryOps, mulDivModOps, addSubOps, comparisonOps, equalOps, andOp, orOp)

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
    | importStmt // Added import stmt

  def returns(stmts: List[Stmt]): Boolean =
    stmts.last match
      case IfElse(_, thenStmts, elseStmts) => returns(thenStmts) && returns(elseStmts)
      case WhileDo(_, stmts) => returns(stmts)
      case Scope(stmts) => returns(stmts)
      case Return(_) => true
      case Exit(_) => true
      case Import(_) => false // added import stmt
      case _ => false


  def onceOrEmptyList[A](structure: Parsley[List[A]]) = ((structure) | pure(List.empty[A]))
  }
