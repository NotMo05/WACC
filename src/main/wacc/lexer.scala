package wacc


import parsley.Parsley
import parsley.token.Lexer
import parsley.token.Basic
import parsley.token.descriptions.*
import parsley.syntax.character._


object lexer {
    private val desc = LexicalDesc.plain.copy(
        nameDesc = NameDesc.plain.copy(
            identifierStart = Basic(char => char.isLetter || char == '_'),
            identifierLetter = Basic(char => char.isLetterOrDigit || char == '_')
        ), 
        spaceDesc = SpaceDesc.plain.copy(
            lineCommentStart = "#",
        ),
        symbolDesc = SymbolDesc.plain.copy(
            hardKeywords = Set("read", "exit", "begin", "end", "if", "then", "else", "fi", "skip", "true", "false",
                               "free", "while", "do", "done", "fst", "snd", "newpair", "print", "println", "call", "null"),
            hardOperators = Set("*", "/", "%", "+", "-", ">", ">=", "<", "<=",
                                "==", "!=", "&&", "||", "!", "len", "ord", "chr")
        )
        //textDesc, escapeDesc, numericDesc
    )
    private val lexer = Lexer(desc)

    val boolLiteral = lexer.lexeme(("true" as true) | ("false" as false))
    val intliteral = lexer.lexeme.integer.decimal32[BigInt]
    val stringliteral = lexer.lexeme.string.ascii
    val charliteral = lexer.lexeme.character.ascii
    val nullliteral = lexer.lexeme("null" as null)
    val ident = lexer.lexeme.names.identifier
    val semi = lexer.lexeme.symbol.semi
    val comma = lexer.lexeme.symbol.comma
    val lParen = lexer.lexeme.symbol.openParen
    val rParen = lexer.lexeme.symbol.closingParen 
    val lSquare = lexer.lexeme.symbol.openSquare
    val rSquare = lexer.lexeme.symbol.closingSquare
    val implicits = lexer.lexeme.symbol.implicits
    
    def fully[A](p: Parsley[A]): Parsley[A] = lexer.fully(p)
}
