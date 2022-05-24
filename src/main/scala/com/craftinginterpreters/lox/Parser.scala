package com.craftinginterpreters.lox

import com.craftinginterpreters.lox.TokenType._

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

class ParserError extends RuntimeException
case class Parser(tokens : Array[Token]) {

  val exprs = ListBuffer.empty[Expr]
  private var current = 0
  private val tokenLength = tokens.length

  def parse() : Array[Stmt] =
    try{
    val statements = ArrayBuffer.empty[Stmt]

    while(!isAtEnd()) {
      statements.append(statement())
    }
    statements.toArray
  } catch {
      case _ : ParserError =>
        Array.empty
    }

  private def statement() : Stmt = {
    // if we encounter a print token , it's a print statement
    // else it's an expression statement

    if(matchType(PRINT)){
      printStatement()
    } else {
      expressiomStatement()
    }
  }

  private def printStatement() : Stmt = {
    val expressionResult = expression()
    // a semicolon must come after an expression
    consume(SEMICOLON, "Expect ';' after expression")

    Stmt.Print(expressionResult)
  }

  private def expressiomStatement(): Stmt = {
    val expressionResult = expression()
    // a semicolon must come after an expression
    consume(SEMICOLON, "Expect ';' after expression")
    Stmt.Expression(expressionResult)

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
    } else throw error(peek(),"Expect Expression")

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

  /**
   * It discards tokens until it thinks it has found a statement boundary.
   * After catching a ParseError, we’ll call this and then we are hopefully back in sync.
   * When it works well, we have discarded tokens that would have likely caused cascaded errors anyway,
   * and now we can parse the rest of the file starting at the next statement
   * */
  private def synchronize()= {
    advance()
    def loop(isAtEnd : Boolean) : Unit = {
      if (isAtEnd) {

      } else {
        if (previous().tokenType == SEMICOLON) {} else {
          peek().get.tokenType match {
            case CLASS | FUN | VAR | FOR | IF | WHILE | PRINT | RETURN =>
            case _ => advance()
          }
          loop(isAtEnd)
        }
      }
    }

    loop(isAtEnd())
  }
}
