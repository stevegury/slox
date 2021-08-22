package io.stevegury.slox

class Interpreter {
    private[this] var environment = new Environment()

    def interpret(statements: Seq[Stmt]): Any = {
        var result: Any = null
        try {
            statements.foreach { stmt =>
                result = execute(stmt)
            }
        } catch {
            case error: RuntimeError => Slox.runtimeError(error)
        }
        return result
    }
    
    def execute(stmt: Stmt): Any = {
        if (stmt == null) {
            throw new Exception("Invalid statement!")
        }
        stmt match {
            case PrintStmt(expr) => 
                val value = evaluate(expr)
                println(stringify(value))
                value
            case Expression(expr) => evaluate(expr)
            case VarStmt(name, initializer) => 
                var value: Any = null
                if (initializer != null) {
                    value = evaluate(initializer)
                }
                environment.define(name.lexeme, value)
                return null
            case BlockStmt(statements) =>
                executeBlock(statements, new Environment(environment))
                return null
            case IfStmt(condition, thenBranch, elseBranch) =>
                if (isTruthy(evaluate(condition))) {
                    execute(thenBranch)
                } else if (elseBranch != null) {
                    execute(elseBranch)
                }
                return null
            case WhileStmt(condition, body) =>
                while (isTruthy(evaluate(condition))) {
                    execute(body)
                }
                return null
        }
    }

    def executeBlock(statements: Seq[Stmt], newEnv: Environment): Unit = {
        val previous = environment
        try {
            environment = newEnv
            statements.foreach { stmt =>
                execute(stmt)
            }
        } finally {
            environment = previous
        }
    }

    def evaluate(expr: Expr): Any = {
        expr match {
            case l: Literal => l.value
            case g: Grouping => evaluate(g.expression)
            case u: Unary =>
                val right = evaluate(u.right)
                u.operator.typ match {
                    case BANG => return !isTruthy(right)
                    case MINUS => 
                        checkNumberOperand(u.operator, right)
                        return -(right).asInstanceOf[Double]
                    case o => throw new RuntimeException("Invalid unary opertor " + o)
                }
                return null
            case Variable(tokenName) => environment.get(tokenName)
            case Assign(token, expr) => 
                val value = evaluate(expr)
                environment.assign(token, value)
                return value
            case Logical(left, operator, right) =>
                val leftValue = evaluate(left)
                if (operator.typ == OR) {
                    if (isTruthy(leftValue)) {
                        return leftValue
                    }
                } else {
                    if (!isTruthy(leftValue)) {
                        return leftValue
                    }
                }
                return evaluate(right)
            case b: Binary =>
                val left = evaluate(b.left)
                val right = evaluate(b.right)
                b.operator.typ match {
                    case GREATER => 
                        checkNumberOperands(b.operator, left, right)
                        left.asInstanceOf[Double] > right.asInstanceOf[Double]
                    case GREATER_EQUAL => 
                        checkNumberOperands(b.operator, left, right)
                        left.asInstanceOf[Double] >= right.asInstanceOf[Double]
                    case LESS => 
                        checkNumberOperands(b.operator, left, right)
                        left.asInstanceOf[Double] < right.asInstanceOf[Double]
                    case LESS_EQUAL => 
                        checkNumberOperands(b.operator, left, right)
                        left.asInstanceOf[Double] <= right.asInstanceOf[Double]
                    case BANG_EQUAL => 
                        checkNumberOperands(b.operator, left, right)
                        !isEqual(left, right)
                    case EQUAL_EQUAL => 
                        checkNumberOperands(b.operator, left, right)
                        isEqual(left, right)
                    case MINUS => 
                        checkNumberOperands(b.operator, left, right)
                        left.asInstanceOf[Double] - right.asInstanceOf[Double]
                    case SLASH => 
                        checkNumberOperands(b.operator, left, right)
                        left.asInstanceOf[Double] / right.asInstanceOf[Double]
                    case STAR => 
                        checkNumberOperands(b.operator, left, right)
                        left.asInstanceOf[Double] * right.asInstanceOf[Double]
                    case PLUS => 
                        if (left.isInstanceOf[Double] && right.isInstanceOf[Double]) {
                            left.asInstanceOf[Double] + right.asInstanceOf[Double]
                        } else if (left.isInstanceOf[String] && right.isInstanceOf[String]) {
                            left.asInstanceOf[String] + right.asInstanceOf[String]
                        } else {
                            throw new RuntimeError(b.operator, "Operands must be two numbers or two strings.")
                        }
                    case _ => throw new RuntimeException(s"Inavlid operator ${b.operator} in binary expression")
                }
        }
    }

    private[this] def isTruthy(obj: Any): Boolean = {
        if (obj == null) {
            return false
        }
        if (obj.isInstanceOf[Boolean]) {
            return obj.asInstanceOf[Boolean]
        }
        return true
    }

    private[this] def isEqual(left: Any, right: Any): Boolean = {
        if (left == null && right == null) {
            return true
        }
        if (left == null) {
            return false
        }
        return left.equals(right)
    }

    private[this] def checkNumberOperand(operator: Token, operand: Any): Unit = {
        if (operand.isInstanceOf[Double]) {
            return
        }
        throw new RuntimeError(operator, "Operand must be a number.")
    }

    private[this] def checkNumberOperands(operator: Token, left: Any, right: Any): Unit = {
        if (left.isInstanceOf[Double] && right.isInstanceOf[Double]) {
            return
        }
        throw new RuntimeError(operator, "Operands must be numbers.")
    }

    def stringify(obj: Any): String = {
        if (obj == null) {
            return "nil"
        }
        if (obj.isInstanceOf[Double]) {
            var txt = obj.toString()
            if (txt.endsWith(".0")) {
                txt = txt.substring(0, txt.length - 2)
            }
            return txt
        }
        obj.toString()
    }
}