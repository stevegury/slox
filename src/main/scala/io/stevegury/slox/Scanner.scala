package io.stevegury.slox

import scala.collection.mutable.ArrayBuffer

class Scanner(src: String) {
    private[this] var tokens: Seq[Token] = IndexedSeq.empty
    private[this] var start: Int = 0
    private[this] var current: Int = 0
    private[this] var line : Int = 1

    def scanTockens(): Seq[Token] = {
        while (!isAtEnd()) {
            start = current
            scanTocken()
        }
        tokens = tokens :+ (Token(EOF, "", null, line))
        tokens
    }

    def scanTocken(): Unit = {
        val c = advance()
        c match {
            case '(' => addToken(LEFT_PAREN)
            case ')' => addToken(RIGHT_PAREN)
            case '{' => addToken(LEFT_BRACE)
            case '}' => addToken(RIGHT_BRACE)
            case ',' => addToken(COMMA)
            case '.' => addToken(DOT)
            case '-' => addToken(MINUS)
            case '+' => addToken(PLUS)
            case ';' => addToken(SEMICOLON)
            case '*' => addToken(STAR)

            case '!' => 
                if (matchChar('=')) {
                    addToken(BANG_EQUAL) 
                } else {
                    addToken(BANG)
                }
            case '=' => 
                if (matchChar('=')) {
                    addToken(EQUAL_EQUAL) 
                } else {
                    addToken(EQUAL)
                }
            case '<' => 
                if (matchChar('=')) {
                    addToken(LESS_EQUAL) 
                } else {
                    addToken(LESS)
                }
            case '>' => 
                if (matchChar('=')) {
                    addToken(GREATER_EQUAL) 
                } else {
                    addToken(GREATER)
                }
            case '/' =>
                if (matchChar('/')) {
                    while (peek() != '\n' && !isAtEnd()) {
                        advance()
                    }
                } else {
                    addToken(SLASH)
                }
            case ' ' | '\r' | '\t' => // ignore
            case '\n' => line += 1
            case '"' => string()
            case _ if isDigit(c) => number()
            case _ if isAlpha(c) => identifier()
            
            case _ => 
                // remove later?
                Slox.error(line, "Unexpected character '$c'!")
        }

    }

    def isAtEnd(): Boolean = {
        current >= src.length
    }

    def advance(): Char = {
        val c = src(current)
        current += 1
        c
    }

    def matchChar(expected: Char): Boolean = {
        if (isAtEnd()) {
            return false
        }
        if (src(current) != expected) {
            return false
        }

        current += 1
        true
    }

    def peek(): Char = {
        if (isAtEnd()) {
            0.toChar
        } else {
            src(current)
        }
    }

    def peekNext(): Char = {
        if (current + 1 >= src.length) {
            0.toChar
        } else {
            src(current + 1)
        }
    }

    def addToken(typ: TokenType): Unit = {
        addToken(typ, null)
    }

    def addToken(typ: TokenType, literal: Any): Unit = {
        val txt = src.substring(start, current)
        tokens = tokens :+ Token(typ, txt, literal, line)
    }

    def string(): Unit = {
        while (peek() != '"' && !isAtEnd()) {
            if (peek() == '\n') {
                line += 1
            }
            advance()
        }
        if (isAtEnd()) {
            Slox.error(line, "Unterminated string.")
            return
        }
        advance() // closing '"'

        val value = src.substring(start + 1, current - 1)
        addToken(STRING, value)
    }

    def isDigit(c: Char): Boolean = {
        c >= '0' && c <= '9'
    }

    def number(): Unit = {
        while (isDigit(peek())) {
            advance()
        }
        if (peek() == '.' && isDigit(peekNext())) {
            advance()
            while (isDigit(peek())) {
                advance()
            }
        }

        addToken(NUMBER, src.substring(start, current).toDouble)
    }

    def isAlpha(c: Char): Boolean = {
        (c >= 'a' && c <= 'z') ||
        (c >= 'A' && c <= 'Z') ||
        c == '_'
    }

    def isAlphaNumeric(c: Char): Boolean = {
        isAlpha(c) || isDigit(c)
    }

    def identifier(): Unit = {
        while (isAlphaNumeric(peek())) {
            advance()
        }
        val txt = src.substring(start, current)
        Keyword.list.get(txt) match {
            case Some(typ) => addToken(typ)
            case _ => addToken(IDENTIFIER)
        }
    }
}
