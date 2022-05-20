package com.craftinginterpreters.lox

import com.craftinginterpreters.lox.TokenType._

import scala.collection.mutable.ListBuffer
class ParserError extends RuntimeException
case class Parser(tokens : Array[Token]) {

  val exprs = ListBuffer.empty[Expr]
  private var current = 0
  private val tokenLength = tokens.length

  def parse() = {
    expression()
  }
  private def expression() : Expr = {
    equality()
  }


  private def equality() : Expr = {
    var expr = comparison()
    while(matchType(BANG_EQUAL,EQUAL_EQUAL)){
     val operator = previous()
      val right = comparison()
      expr = Expr.Binary(expr,operator,right)
    }
    expr
  }
  private def comparison() : Expr = {
    var expr = term()
    while(matchType(GREATER,GREATER_EQUAL,LESS,LESS_EQUAL)){
      val operator = previous()
      val right = term()
      expr = Expr.Binary(expr, operator,right)
    }
    expr
  }
  private def term() : Expr = {
    var expr = factor()
    while(matchType(MINUS,PLUS)){
      val operator = previous()
      val right = factor()
      expr = Expr.Binary(expr,operator,right)
    }
    expr
  }

  private def factor() : Expr = {
    var expr = unary()
    while(matchType(SLASH,STAR)){
      val operator = previous()
      val right =  unary()
      expr = Expr.Binary(expr,operator,right)
    }
    expr
  }

  private def unary() : Expr = {
    if(matchType(BANG,MINUS)){
      val operator = previous()
      val right = unary()
      Expr.Unary(operator,right)
    }else {
      primary()
    }
  }

  private def primary() : Expr = {
    if (matchType(FALSE)){
      Expr.Literal(false)
    } else if (matchType(TRUE)){
      Expr.Literal(true)
    } else if (matchType(NIL)){
      Expr.Literal(null)
    } else if (matchType(NUMBER,STRING)){
      Expr.Literal(previous().literal)
    } else if(matchType(LEFT_PAREN)){
      val expr = expression()
      consume(RIGHT_PAREN,"Expect ')' after expression")
      Expr.Grouping(expr)
    } else throw new Exception("Cannot match Primary")

  }

  private def consume(rightparen: TokenType, msg: String): Unit ={
    if(peek().map(_.tokenType).contains(rightparen)){
      advance()
    } else throw error(peek(),msg)

  }

  private def error(token : Option[Token], message : String) = {
    Lox.error(token,message)
    new ParserError
  }

  private def matchType(types : TokenType*) : Boolean = {
    if(types.contains(SLASH)) {
      println("got a slash")
    }
    val res = peek().map(_.tokenType).exists(types.contains)
    if(res) advance()
    res
  }

  private def advance() = {
    if(!isAtEnd()) {
      current += 1
    }
    previous()
  }

  private def previous() = {
    tokens(current - 1)
  }



  private def isAtEnd() = {
    current >= tokenLength - 1
  }

  private def peek(): Option[Token] = {
    Option.when(!isAtEnd())(tokens(current))
  }
}
