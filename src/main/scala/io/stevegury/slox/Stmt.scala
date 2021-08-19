package io.stevegury.slox

sealed trait Stmt
case class PrintStmt(value: Expr) extends Stmt
case class Expression(value: Expr) extends Stmt
case class VarStmt(name: Token, initializer: Expr) extends Stmt
case class BlockStmt(stmts: Seq[Stmt]) extends Stmt