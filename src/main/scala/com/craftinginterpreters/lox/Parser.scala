package com.craftinginterpreters.lox

import com.craftinginterpreters.lox.TokenType._

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

class ParserError extends RuntimeException
case class Parser(tokens : Array[Token]) {

  val exprs = ListBuffer.empty[Expr]
  private var current = 0
  private val tokenLength = tokens.length

  /**
   * program        → declaration* EOF ;

      declaration    → varDecl
                     | statement ;

      statement      → exprStmt
                      | printStmt ;
                      | block ;
   * */
  def parse() : Array[Stmt] = {
    val statements = ArrayBuffer.empty[Stmt]
    while (!isAtEnd()) {
      statements.append(declaration())
    }
    statements.toArray
  }

  private def declaration() = try{
    if(matchAndConsumeType(VAR)){
      varDeclaration()
    } else {
      statement()
    }
  } catch {
    case _ : ParserError =>
      synchronize()
      null
  }

  private def varDeclaration() :Stmt = {

    // tries to match
    // var name  = "samuel ;
    //   or
    // var name;
    val token = consume(IDENTIFIER, "Expect variable name")
    var variableExpression : Expr = null
    if(matchAndConsumeType(EQUAL)){
      variableExpression = expression()

    }
    consume(SEMICOLON, "Expect ';' after variable declaration.");
    Stmt.Var(token,variableExpression)
  }

  private def statement() : Stmt = {
    // if we encounter a print token , it's a print statement
    // else it's an expression statement

    /**
     * statement      → exprStmt
                      | printStmt
                      | block ;
    block          → "{" declaration* "}" ;
     *
     * */
    if(matchAndConsumeType(PRINT)){
      printStatement()
    } else if (matchAndConsumeType(LEFT_BRACE)){
     Stmt.Block(block())
    }else {
      expressionStatement()
    }
  }

  private def check(tokenType: TokenType) : Boolean = {
    peek().exists(_.tokenType == tokenType)
  }

  private def block() : List[Stmt] = {
    //block          → "{" declaration* "}" ;

    val statements  = ListBuffer.empty[Stmt]

    // while it's not a right brace and is not at the end of the file

    while(!check(RIGHT_BRACE) && !isAtEnd()){
      statements.addOne(declaration())
    }
    consume(RIGHT_BRACE,"Expect a '}' after block. ")

    statements.toList
  }
  private def printStatement() : Stmt = {
    val expressionResult = expression()
    // a semicolon must come after an expression
    consume(SEMICOLON, "Expect ';' after expression")

    Stmt.Print(expressionResult)
  }

  private def expressionStatement(): Stmt = {
    val expressionResult = expression()
    // a semicolon must come after an expression
    consume(SEMICOLON, "Expect ';' after expression")
    Stmt.Expression(expressionResult)

  }
  private def expression() : Expr = {
    assignment()
  }

  private def assignment() : Expr = {

    // gotta be able to distinguish between
    // var a = 4  CORRECT
    // a + b = c  Not correct
    val leftHand = equality()

    if (matchAndConsumeType(EQUAL)){
      val equals = previous()
      leftHand match {
        case e : Expr.Variable =>
          val right = assignment()
          Expr.Assign(e.name, right)
        case _ =>
          error(Some(equals), "Invalid assignment target.")
          leftHand
      }
    } else {
      leftHand
    }
  }


  private def equality() : Expr = {
    var expr = comparison()
    while(matchAndConsumeType(BANG_EQUAL,EQUAL_EQUAL)){
     val operator = previous()
      val right = comparison()
      expr = Expr.Binary(expr,operator,right)
    }
    expr
  }
  private def comparison() : Expr = {
    var expr = term()
    while(matchAndConsumeType(GREATER,GREATER_EQUAL,LESS,LESS_EQUAL)){
      val operator = previous()
      val right = term()
      expr = Expr.Binary(expr, operator,right)
    }
    expr
  }
  private def term() : Expr = {
    var expr = factor()
    while(matchAndConsumeType(MINUS,PLUS)){
      val operator = previous()
      val right = factor()
      expr = Expr.Binary(expr,operator,right)
    }
    expr
  }

  private def factor() : Expr = {
    var expr = unary()
    while(matchAndConsumeType(SLASH,STAR)){
      val operator = previous()
      val right =  unary()
      expr = Expr.Binary(expr,operator,right)
    }
    expr
  }

  private def unary() : Expr = {
    if(matchAndConsumeType(BANG,MINUS)){
      val operator = previous()
      val right = unary()
      Expr.Unary(operator,right)
    }else {
      primary()
    }
  }

  private def primary() : Expr = {
    if (matchAndConsumeType(FALSE)){
      Expr.Literal(false)
    } else if (matchAndConsumeType(TRUE)){
      Expr.Literal(true)
    } else if (matchAndConsumeType(NIL)){
      Expr.Literal(null)
    } else if (matchAndConsumeType(NUMBER,STRING)){
      Expr.Literal(previous().literal)
    } else if (matchAndConsumeType(IDENTIFIER)){
      Expr.Variable(previous())
    } else if(matchAndConsumeType(LEFT_PAREN)){
      val expr = expression()
      consume(RIGHT_PAREN,"Expect ')' after expression")
      Expr.Grouping(expr)
    } else throw error(peek(),"Expect Expression")

  }

  private def consume(expected: TokenType, msg: String) : Token={
    if(peek().map(_.tokenType).contains(expected)){
      advance()
    } else throw error(peek(),msg)

  }

  private def error(token : Option[Token], message : String) = {
    Lox.error(token,message)
    new ParserError
  }

  private def matchAndConsumeType(types : TokenType*) : Boolean = {
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
