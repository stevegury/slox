package io.stevegury.slox

sealed trait Expr

case class Binary(left: Expr, operator: Token, right: Expr) extends Expr {
    override def toString(): String = {
        s"(${operator.lexeme} $left $right)"
    }
}

case class Grouping(expression: Expr) extends Expr {
    override def toString(): String = {
        s"($expression)"
    }
}

case class Literal(value: Any) extends Expr {
    override def toString(): String = {
        value match {
            case _: String => s"\"$value\""
            case _ => s"$value"
        }
    }
}

case class Unary(operator: Token, right: Expr) extends Expr {
    override def toString(): String = {
        s"(${operator.lexeme} $right)"
    }
}
