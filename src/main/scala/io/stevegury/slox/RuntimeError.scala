package io.stevegury.slox

class RuntimeError(val token: Token, message: String) extends RuntimeException(message)