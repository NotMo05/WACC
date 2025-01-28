package wacc

import parsley.Parsley
import parsley.Parsley.atomic
import parsley.syntax.character.stringLift
import parsley.token.{Lexer, Basic}
import parsley.token.descriptions._

object lexer {
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
                          "int", "bool", "pair", "null", "string", "pair"),
      hardOperators = Set("*", "/", "%", "+", "-", ">", ">=", "<", "<=",
                          "==", "!=", "&&", "||", "!", "len", "ord", "chr")
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
      )
    )
  )

  private val lexer = Lexer(desc)

  val boolLiteral = BoolLiteral(lexer.lexeme(atomic("true" as true) | atomic("false" as false)))
  val intLiteral = IntLiteral(lexer.lexeme.integer.decimal32[BigInt])
  val stringLiteral = StringLiteral(lexer.lexeme.string.ascii)
  val charLiteral = CharLiteral(lexer.lexeme.character.ascii)
  val nullLiteral = lexer.lexeme(atomic("null" as NullLiteral))
  val skipStmt = lexer.lexeme(atomic("skip" as Skip))
  val ident = Ident(lexer.lexeme.names.identifier)
  val implicits = lexer.lexeme.symbol.implicits
  val typeParser: Parsley[Type] = lexer.lexeme(
      atomic("int" as IntType)
    | atomic("char" as CharType)
    | atomic("bool" as BoolType)
    | atomic("string" as StringType)
    //pair and arrays?
  )

  val fstOrSnd: Parsley[Pos] = lexer.lexeme(
      atomic("fst" as Fst)
    | atomic("snd" as Snd)
  )

  def fully[A](p: Parsley[A]): Parsley[A] = lexer.fully(p)
}