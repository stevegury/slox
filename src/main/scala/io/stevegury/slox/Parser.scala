package io.stevegury.slox

object Parser {
    class ParseError extends RuntimeException
}

class Parser(tokens: IndexedSeq[Token]) {
    import Parser._
    
    private[this] var current: Int = 0

    def parse(): Seq[Stmt] = {
        var statements = Seq.empty[Stmt]
        while (!isAtEnd()) {
            val s = declaration()
            if (s != null) {
                statements = statements :+ s
            }
        }
        statements
    }

    private[this] def declaration(): Stmt = {
        try {
            if (matchToken(VAR)) {
                return varDeclaration()
            }
            return statement()
        } catch {
            case _: ParseError =>
                synchronize()
                return null
        }
    }

    private[this] def varDeclaration(): Stmt = {
        val tokenName = consume(IDENTIFIER, "Expect variable name.")

        var initializer: Expr = null
        if (matchToken(EQUAL)) {
            initializer = expression()
        }

        consume(SEMICOLON, "Expect ';' after variable declaration.")
        return VarStmt(tokenName, initializer)
    }

    private[this] def statement(): Stmt = {
        if (matchToken(FOR)) {
            return forStatement()
        }
        if (matchToken(IF)) {
            return ifStatement()
        }
        if (matchToken(PRINT)) {
            return printStatement()
        }
        if (matchToken(WHILE)) {
            return whileStatement()
        }
        if (matchToken(LEFT_BRACE)) {
            return BlockStmt(block())
        }
        expressionStatement()
    }

    private[this] def forStatement(): Stmt = {
        consume(LEFT_PAREN, "Expect '(' after 'for'.")
        
        var initializer: Stmt = null
        if (matchToken(SEMICOLON)) {
            initializer = null
        } else if (matchToken(VAR)) {
            initializer = varDeclaration()
        } else {
            initializer = expressionStatement()
        }

        var condition: Expr = null
        if (!matchToken(SEMICOLON)) {
            condition = expression()
        }
        consume(SEMICOLON, "Expect ';' after loop condition.")

        var increment: Expr = null
        if (!check(RIGHT_PAREN)) {
            increment = expression()
        }
        consume(RIGHT_PAREN, "Expect ')' after for clauses.")

        var body = statement()

        if (increment != null) {
            body = BlockStmt(Seq(body, Expression(increment)))
        }

        if (condition == null) {
            condition = Literal(true)
        }
        body = WhileStmt(condition, body)

        if (initializer != null) {
            body = BlockStmt(Seq(initializer, body))
        }

        body
    }

    private[this] def ifStatement(): Stmt = {
        consume(LEFT_PAREN, "Expect '(' after if.")
        val condition = expression()
        consume(RIGHT_PAREN, "Expect ')'' after if condition.")

        val thenBranch = statement()
        var elseBranch: Stmt = null
        if (matchToken(ELSE)) {
            elseBranch = statement()
        }

        IfStmt(condition, thenBranch, elseBranch)
    }

    private[this] def printStatement(): Stmt = {
        val expr = expression()
        consume(SEMICOLON, "Expect ';' after value.")
        PrintStmt(expr)
    }

    private[this] def whileStatement(): Stmt = {
        consume(LEFT_PAREN, "Expect '(' after while.")
        val condition = expression()
        consume(RIGHT_PAREN, "Expect ')' after while condition.")
        val body = statement()
        WhileStmt(condition, body)
    }

    private[this] def block(): Seq[Stmt] = {
        var statements = Seq.empty[Stmt]

        while (!check(RIGHT_BRACE) && !isAtEnd()) {
            statements = statements :+ declaration()
        }

        consume(RIGHT_BRACE, "Expect '}' after block.")
        statements
    }

    private[this] def expressionStatement(): Stmt = {
        val expr = expression()
        consume(SEMICOLON, "Expect ';' after expression.")
        Expression(expr)
    }

    private[this] def expression(): Expr = {
        assignement()
    }

    private[this] def assignement(): Expr = {
        val expr = or()
        if (matchToken(EQUAL)) {
            val equals = previous()
            val value = assignement()

            expr match {
                case Variable(tokenName) => return Assign(tokenName, value)
                case _ => error(equals, "Invalid assignement target.")
            }
        }
        return expr
    }

    private[this] def or(): Expr = {
        var expr = and()
        while (matchToken(OR)) {
            val operator = previous()
            val right = and()
            expr = Logical(expr, operator, right)
        }
        expr
    }

    private[this] def and(): Expr = {
        var expr = equality()
        while (matchToken(AND)) {
            val operator = previous()
            val right = equality()
            expr = Logical(expr, operator, right)
        }
        expr
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

        if (matchToken(IDENTIFIER)) {
            return Variable(previous())
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

    private[this] def advance(): Token = {
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

    private[this] def consume(typ: TokenType, msg: String): Token = {
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