package com.craftinginterpreters.lox

object AstPrinter  extends Expr.Visitor[String]{


  override def visitAssignExpr(expr: Expr.Assign): String = ???

  override def visitVariableExpr(expr: Expr.Variable): String = ???

  private def parenthesize(name : String, expr : Expr*) : String = {
    val builder = new StringBuilder()
    builder.append("(").append(name)
    expr.foreach {exp =>
      builder.append(" ")
      .append(exp.accept(this))
    }

    builder.append(")")


    builder.toString()
  }
  def print(expr : Expr) : String = {
    expr.accept(this)
  }
  override def visitBinaryExpr(expr: Expr.Binary): String = {
    parenthesize(expr.operator.lexeme,expr.left,expr.right)
  }

  override def visitGroupingExpr(expr: Expr.Grouping): String = {
    parenthesize("group",expr.expression)
  }

  override def visitLiteralExpr(expr: Expr.Literal): String = {
    if (expr.value == null) {
      "nil"
    } else {
      expr.value.toString
    }
  }

  override def visitUnaryExpr(expr: Expr.Unary): String = {
    parenthesize(expr.operator.lexeme,expr.right)
  }
}

object Player extends App {
  val expression = new Expr.Binary(
    new Expr.Unary(
      new Token(TokenType.MINUS, "-", null, 1),
      new Expr.Literal(123)),
    new Token(TokenType.STAR, "*", null, 1),
    new Expr.Grouping(
      new Expr.Literal(45.67)));


  print(AstPrinter.print(expression))
}
