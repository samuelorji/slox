package com.craftinginterpreters.lox

trait Stmt {
 import Stmt.Visitor
 def accept[A](visitor : Visitor[A]) : A 
}
object Stmt {
 trait Visitor[A] { 
  def visitIfStmt(stmt: If) : A
  def visitBlockStmt(stmt: Block) : A
  def visitFunctionStmt(stmt: Function) : A
  def visitExpressionStmt(stmt: Expression) : A
  def visitPrintStmt(stmt: Print) : A
  def visitWhileStmt(stmt: While) : A
  def visitReturnStmt(stmt: Return) : A
  def visitVarStmt(stmt: Var) : A
  }
  final case class If(condition : Expr, thenBranch : Stmt, elseBranch : Option[Stmt]) extends Stmt {
    override def accept[A](visitor : Visitor[A]): A = visitor.visitIfStmt(this)
 }
  final case class Block(statements : List[Stmt]) extends Stmt {
    override def accept[A](visitor : Visitor[A]): A = visitor.visitBlockStmt(this)
 }
  final case class Function(name : Token, params : List[Token] , body : List[Stmt]) extends Stmt {
    override def accept[A](visitor : Visitor[A]): A = visitor.visitFunctionStmt(this)
 }
  final case class Expression(expression : Expr) extends Stmt {
    override def accept[A](visitor : Visitor[A]): A = visitor.visitExpressionStmt(this)
 }
  final case class Print(expression : Expr) extends Stmt {
    override def accept[A](visitor : Visitor[A]): A = visitor.visitPrintStmt(this)
 }
  final case class While(expression : Expr, statement : Stmt) extends Stmt {
    override def accept[A](visitor : Visitor[A]): A = visitor.visitWhileStmt(this)
 }
  final case class Return(keyword : Token, value : Option[Expr]) extends Stmt {
    override def accept[A](visitor : Visitor[A]): A = visitor.visitReturnStmt(this)
 }
  final case class Var(name : Token, initializer : Expr) extends Stmt {
    override def accept[A](visitor : Visitor[A]): A = visitor.visitVarStmt(this)
 }
  final case class AnonFunction( params : List[Token] , body : List[Stmt]) extends Stmt {
    override def accept[A](visitor: Visitor[A]): A = ???
  }
}
