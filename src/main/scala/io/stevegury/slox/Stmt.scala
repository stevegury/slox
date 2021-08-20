package io.stevegury.slox

sealed trait Stmt
case class PrintStmt(value: Expr) extends Stmt
case class Expression(value: Expr) extends Stmt
case class VarStmt(name: Token, initializer: Expr) extends Stmt
case class BlockStmt(stmts: Seq[Stmt]) extends Stmt
case class IfStmt(condition: Expr, thenBranch: Stmt, elseBranch: Stmt) extends Stmt
case class WhileStmt(condition: Expr, body: Stmt) extends Stmt
