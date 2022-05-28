package com.craftinginterpreters.lox

trait Expr {
 import Expr.Visitor
 def accept[A](visitor : Visitor[A]) : A 
}
object Expr {
 trait Visitor[A] { 
  def visitAssignExpr(expr: Assign) : A
  def visitBinaryExpr(expr: Binary) : A
  def visitGroupingExpr(expr: Grouping) : A
  def visitLiteralExpr(expr: Literal) : A
  def visitUnaryExpr(expr: Unary) : A
  def visitVariableExpr(expr: Variable) : A
  }
  final case class Assign(name : Token, value : Expr) extends Expr {
    override def accept[A](visitor : Visitor[A]): A = visitor.visitAssignExpr(this)
 }
  final case class Binary(left : Expr, operator : Token, right : Expr) extends Expr {
    override def accept[A](visitor : Visitor[A]): A = visitor.visitBinaryExpr(this)
 }
  final case class Grouping(expression : Expr) extends Expr {
    override def accept[A](visitor : Visitor[A]): A = visitor.visitGroupingExpr(this)
 }
  final case class Literal(value : Any) extends Expr {
    override def accept[A](visitor : Visitor[A]): A = visitor.visitLiteralExpr(this)
 }
  final case class Unary(operator : Token, right : Expr) extends Expr {
    override def accept[A](visitor : Visitor[A]): A = visitor.visitUnaryExpr(this)
 }
  final case class Variable(name : Token) extends Expr {
    override def accept[A](visitor : Visitor[A]): A = visitor.visitVariableExpr(this)
 }
}
