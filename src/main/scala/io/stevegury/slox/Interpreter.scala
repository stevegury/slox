package io.stevegury.slox

class Interpreter {


    def interpret(expr: Expr): Any = {
        try {
            val value = evaluate(expr)
            // println(stringify(value))
            return value
        } catch {
            case error: RuntimeError => Slox.runtimeError(error)
        }
        return null
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
        throw new RuntimeError(operator, "Operands must be a number.")
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