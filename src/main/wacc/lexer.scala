package wacc

import parsley.Parsley
import parsley.token.Lexer
import parsley.token.Basic
import parsley.token.descriptions.*

object lexer {
    private val desc = LexicalDesc.plain.copy(
        nameDesc = NameDesc.plain.copy(
            identifierStart = Basic(_.isLetter), // or underscore
            identifierLetter = Basic(_.isLetterOrDigit) // or underscore
        ),
        spaceDesc = SpaceDesc.plain.copy(
            lineCommentStart = "#",
        ),
        symbolDesc = SymbolDesc.plain.copy(
           // hardKeywords = Set("read", "exit", "begin", "end", "if", "then", "else", "fi", "skip",
                               //"free", "while", "do", "done", "fst", "snd", "newpair", "print", "println"), // Not too sure on the prints but I think yes
            hardKeywords = Set("if"),
            hardOperators = Set("*", "/", "%", "+", "-", ">", ">=", "<", "<=",
                                "==", "!=", "&&", "||", "!", "len", "ord", "chr")
        )
        //textDesc, escapeDesc, numericDesc
    )
    private val lexer = Lexer(desc)

    val intliteral = lexer.lexeme.integer.decimal32[BigInt]
    val ident = lexer.lexeme.names.identifier
    val implicits = lexer.lexeme.symbol.implicits
    def fully[A](p: Parsley[A]): Parsley[A] = lexer.fully(p)
}
