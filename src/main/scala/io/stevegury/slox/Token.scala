package io.stevegury.slox

case class Token(typ: TokenType, lexeme: String, literal: Any, line: Int)