package com.craftinginterpreters.lox

trait Expr {
 import Expr.Visitor
 def accept[A](visitor : Visitor[A]) : A 
}
object Expr {
 trait Visitor[A] { 
  def visitBinaryExpr(expr: Binary) : A
  def visitGroupingExpr(expr: Grouping) : A
  def visitLiteralExpr(expr: Literal) : A
  def visitUnaryExpr(expr: Unary) : A
  }
  case class Binary(left : Expr, operator : Token, right : Expr) extends Expr {
    override def accept[A](visitor : Visitor[A]): A = visitor.visitBinaryExpr(this)
 }
  case class Grouping(expression : Expr) extends Expr {
    override def accept[A](visitor : Visitor[A]): A = visitor.visitGroupingExpr(this)
 }
  case class Literal(value : Any) extends Expr {
    override def accept[A](visitor : Visitor[A]): A = visitor.visitLiteralExpr(this)
 }
  case class Unary(operator : Token, right : Expr) extends Expr {
    override def accept[A](visitor : Visitor[A]): A = visitor.visitUnaryExpr(this)
 }
}
