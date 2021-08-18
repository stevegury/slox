package io.stevegury.slox

class Parser(tokens: IndexedSeq[Token]) {
    private class ParseError extends RuntimeException
    
    private[this] var current: Int = 0

    def parse(): Expr = {
        try {
            expression()
        } catch {
            case _: ParseError => null
        }
    }

    private[this] def expression(): Expr = {
        equality()
    }

    private[this] def equality(): Expr = {
        var expr = comparison()
        while (matchToken(BANG_EQUAL, EQUAL_EQUAL)) {
            val operator = previous()
            val right = comparison()
            expr = Binary(expr, operator, right)
        }
        expr
    }

    private[this] def comparison(): Expr = {
        var expr = term()
        while (matchToken(GREATER, GREATER_EQUAL, LESS, LESS_EQUAL)) {
            val operator = previous()
            val right = term()
            expr = Binary(expr, operator, right)
        }
        expr
    }

    private[this] def term(): Expr = {
        var expr = factor()
        while (matchToken(MINUS, PLUS)) {
            val operator = previous()
            val right = factor()
            expr = Binary(expr, operator, right)
        }
        expr
    }

    private[this] def factor(): Expr = {
        var expr = unary()
        while (matchToken(STAR, SLASH)) {
            val operator = previous()
            val right = unary()
            expr = Binary(expr, operator, right)
        }
        expr
    }

    private[this] def unary(): Expr = {
        if (matchToken(BANG, MINUS)) {
            val operator = previous()
            val right = unary()
            return Unary(operator, right)
        }
        primary()
    }

    private[this] def primary(): Expr = {
        if (matchToken(FALSE)) {
            return Literal(false)
        }
        if (matchToken(TRUE)) {
            return Literal(true)
        }
        if (matchToken(NIL)) {
            return Literal(null)
        }

        if (matchToken(NUMBER, STRING)) {
            return Literal(previous().literal)
        }

        if (matchToken(LEFT_PAREN)) {
            val expr = expression()
            consume(RIGHT_PAREN, "Expect ')' after expression.")
            return Grouping(expr)
        }

        throw error(peek(), "Expected expression")
    }

    private[this] def synchronize(): Unit = {
        advance()
        while (!isAtEnd()) {
            if (previous().typ == SEMICOLON) {
                return
            }

            peek().typ match {
                case CLASS | FOR | FUN | IF | PRINT | RETURN | VAR | WHILE => return
                case _ => advance()
            }
        }
    }

    private[this] def matchToken(types: TokenType *): Boolean = {
        types.foreach { typ =>
            if (check(typ)) {
                advance()
                return true
            }
        }
        false
    }

    private[this] def check(typ: TokenType): Boolean = {
        if (isAtEnd()) {
            return false
        }
        peek().typ == typ
    }

    private[this] def advance(): Unit = {
        if (!isAtEnd()) {
            current += 1
        }
        previous()
    }

    private[this] def isAtEnd(): Boolean = {
        peek().typ == EOF
    }

    private[this] def peek(): Token = {
        tokens(current)
    }

    private[this] def previous(): Token = {
        tokens(current - 1)
    }

    private[this] def consume(typ: TokenType, msg: String): Unit = {
        if (check(typ)) {
            return advance()
        }
        throw error(peek(), msg)
    }

    private[this] def error(token: Token, msg: String): ParseError = {
        Slox.error(token, msg)
        return new ParseError();
    }
}