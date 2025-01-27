package wacc

import parsley.Parsley
import parsley.Parsley._
import parsley.token.Lexer
import parsley.token.Basic
import parsley.token.descriptions.*
import parsley.syntax.character._


def isEnglishLetter(c: Char): Boolean = ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || (c == '_')

object lexer {
    private val desc = LexicalDesc.plain.copy(
        nameDesc = NameDesc.plain.copy(
            identifierStart  =  Basic(char => char.isLetter),
            identifierLetter = Basic(char => char.isLetter || char.isDigit),
            
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
    )

    private val lexer = Lexer(desc)

    val boolLiteral = lexer.lexeme(atomic("true" as true) | atomic("false" as false))
    val intLiteral = lexer.lexeme.integer.decimal32[BigInt]
    val stringLiteral = lexer.lexeme.string.ascii
    val charLiteral = lexer.lexeme.character.ascii
    val nullLiteral = lexer.lexeme(atomic("null" as null))
    val ident = lexer.lexeme.names.identifier
    val semi = lexer.lexeme.symbol.semi
    val comma = lexer.lexeme.symbol.comma
    val lParen = lexer.lexeme.symbol.openParen
    val rParen = lexer.lexeme.symbol.closingParen 
    val lSquare = lexer.lexeme.symbol.openSquare
    val rSquare = lexer.lexeme.symbol.closingSquare
    val implicits = lexer.lexeme.symbol.implicits
    
    def fully[A](p: Parsley[A]): Parsley[A] = lexer.fully(p)

    val intType = lexer.lexeme(atomic("int" as IntType))
}