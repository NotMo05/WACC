package wacc

import parsley.Parsley
import parsley.Parsley.{atomic, some, notFollowedBy}
// import parsley.syntax.character.stringLift
import parsley.token.{Lexer, Basic}
import parsley.token.descriptions._
import wacc.parser.pairType
import parsley.character.string


object lexer {
  val illegalCharacters = Set('\'', '\\', '\"')
  private def isEnglishLetter(c: Char): Boolean =
    ('a' <= c && c <= 'z') ||
    ('A' <= c && c <= 'Z') ||
    (c == '_')

  private val desc = LexicalDesc.plain.copy(
    nameDesc = NameDesc.plain.copy(
      identifierStart  =  Basic(char => isEnglishLetter(char)),
      identifierLetter = Basic(char => isEnglishLetter(char) || char.isDigit),
    ),
    spaceDesc = SpaceDesc.plain.copy(
      lineCommentStart = "#",
    ),
    symbolDesc = SymbolDesc.plain.copy(
      hardKeywords  = Set("read", "exit", "begin", "end", "if", "then", "else", "fi", "skip", "true", "false",
                          "free", "while", "do", "done", "fst", "snd", "newpair", "print", "println", "call",
                          "int", "bool", "char", "null", "string", "pair", "return", "len", "ord", "chr"),
      hardOperators = Set("*", "/", "%", "+", "-", ">", ">=", "<", "<=",
                          "==", "!=", "&&", "||", "!")
    ),
    textDesc = TextDesc.plain.copy(
      escapeSequences = EscapeDesc.plain.copy(
        literals = Set('\"', '\\', '\''),
        mapping = Map(
          "n" -> 0x0a,
          "t" -> 0x09,
          "b" -> 0x08,
          "r" -> 0x0d,
          "f" -> 0x0c,
          "0" -> 0x00
        )
      ),
      graphicCharacter = Basic(c => (c >= ' ' && !illegalCharacters.contains(c)))
    )
  )

  private val lexer = Lexer(desc)


  val boolLiteral = BoolLiteral(lexer.lexeme(atomic(string("true") as true) | atomic(string("false") as false)))
  val integer = lexer.lexeme.integer.decimal32[BigInt]
  val intLiteral: Parsley[Expr] = IntLiteral(integer)


  val stringLiteral = StringLiteral(lexer.lexeme.string.ascii)
  val charLiteral = CharLiteral(lexer.lexeme.character.ascii)
  val nullLiteral = lexer.lexeme(atomic(string("null") as NullLiteral))
  val skipStmt = lexer.lexeme((string("skip") as Skip))
  val ident = Ident(lexer.lexeme.names.identifier)
  val comma = lexer.lexeme.symbol.comma
  val implicits = lexer.lexeme.symbol.implicits


  def fully[A](p: Parsley[A]): Parsley[A] = lexer.fully(p)
}