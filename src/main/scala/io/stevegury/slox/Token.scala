package io.stevegury.slox

case class Token(typ: TokenType, lexeme: String, literal: Any, line: Int) {
    override def toString(): String = {
        typ match {
            case IDENTIFIER => s"IDENTIFIER($lexeme)"
            case STRING => s"STRING($lexeme)"
            case NUMBER => s"NUMBER($lexeme)"
            case _ => typ.toString
        }
    }
}
