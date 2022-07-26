package com.craftinginterpreters.lox

import com.craftinginterpreters.lox.TokenType._

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

class ParserError extends RuntimeException
case class Parser(tokens : Array[Token]) {

  var count = 0
  val exprs = ListBuffer.empty[Expr]
  private var current = 0
  private val tokenLength = tokens.length

  /**
   * program        → declaration* EOF ;

      declaration    → funDecl
                    | varDecl
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

    /**
    declaration    → classDecl
               | funDecl
               | varDecl
               | statement ;

      classDecl      → "class" IDENTIFIER "{" function* "}" ;
     * */

    if(matchAndConsumeType(CLASS)){
      classDeclaration()
    }
    else if(matchAndConsumeType(VAR)){
      varDeclaration()
    } else if (matchAndConsumeType(FUN)) {
      functionDeclaration("function")
    }else {
      statement()
    }
  } catch {
    case e : ParserError =>
      println(s"parser error : ${e}")
      synchronize()
      null
  }

  private def classDeclaration() : Stmt = {
  //  classDecl      → "class" IDENTIFIER ( "<" IDENTIFIER )?
    //                 "{" function* "}" ;

    var superClass : Option[Expr.Variable] = None
    val identifier = consume(IDENTIFIER,"expect a class name ")
    if(matchAndConsumeType(LESS)){
      val superClassToken= consume(IDENTIFIER,"Expect a superclass name after '<'")
      superClass = Some(Expr.Variable(superClassToken))


    }
    consume(LEFT_BRACE,"expect a '{' after class name")
    val methods = ListBuffer.empty[Stmt.Function]

    while(!check(RIGHT_BRACE) && !isAtEnd()){
      methods.addOne(functionDeclaration("method"))
    }

    consume(RIGHT_BRACE, "expect a '}' after class body")

    Stmt.Class(identifier,methods.toList,superClass)

  }
  private def functionDeclaration(kind : String) : Stmt.Function = {
    /**
     * funDecl        → "fun" function ;
       function       → IDENTIFIER "(" parameters? ")" block ;
     * */

      /**
       * parameters     → IDENTIFIER ( "," IDENTIFIER )* ;
       * */
    // fun has been consumed, next should be the function

    val params  = ListBuffer.empty[Token]
    val identifier =  consume(IDENTIFIER, s"Expected a $kind name after 'fun'")
    consume(LEFT_PAREN, "Expected a '(' after function name")

    if(!check(RIGHT_PAREN)){
      do {
        if(params.size > 255){
          error(peek(), "Cannot have more than 255 arguments")
        }
        params.addOne(consume(IDENTIFIER, "Expect parameter name"))
      } while (matchAndConsumeType(COMMA))
    }

    consume(RIGHT_PAREN,  "Expect ')' after parameters.")
    consume(LEFT_BRACE, "Expect '{' before " + kind + " body.")


    val body = block()
    Stmt.Function(identifier,params.toList,body)

  }

//  private def anonFunc() : Stmt.AnonFunction = {
//    val params  = ListBuffer.empty[Token]
//    consume(LEFT_PAREN, "Expected a '(' after function name")
//
//    if(!check(RIGHT_PAREN)){
//      do {
//        if(params.size > 255){
//          error(peek(), "Cannot have more than 255 arguments")
//        }
//        params.addOne(consume(IDENTIFIER, "Expect parameter name"))
//      } while (matchAndConsumeType(COMMA))
//    }
//
//    consume(RIGHT_PAREN,  "Expect ')' after parameters.")
//    consume(LEFT_BRACE, "Expect '{' before function" + " body.")
//
//
//    val body = block()
//    Stmt.AnonFunction(params.toList,body)
//  }

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
                     | forStmt
                     | ifStmt
                     | printStmt
                     | whileStmt
                     | block ;
                     | returnStmt
    returnStmt     → "return" expression? ";" ;
     * */

    if(matchAndConsumeType(IF)){
      ifStatement()
    } else if (matchAndConsumeType(FOR)){
      forStatement()
    } else if (matchAndConsumeType(RETURN)){
      returnStatement()
    }
    else if(matchAndConsumeType(WHILE)){
      whileStatement()
    }
    else if(matchAndConsumeType(PRINT)){
      printStatement()
    } else if (matchAndConsumeType(LEFT_BRACE)){
     Stmt.Block(block())
    }else {
      expressionStatement()
    }
  }

  private def returnStatement() : Stmt = {

    /**
     * returnStmt     → "return" expression? ";" ;
     * */
    val returnToken = previous()

    var returnExpr : Option[Expr] = None

    if(!check(SEMICOLON)){
      // if a semicolon doesn't follow, then we can evaluate the
      // expression  to get the return value
      returnExpr = Some(expression())
    }

    consume(SEMICOLON, "Expect a ';' after a return value")
    Stmt.Return(returnToken, returnExpr)
  }
  private def forStatement() : Stmt = {
     // for (initialize ;condition ; statement )
     // any could be ommitted
    consume(LEFT_PAREN,"Expect '(' after 'for'")

    var initializer : Option[Stmt] = None
    if(matchAndConsumeType(VAR)){
      initializer = Some(varDeclaration())
    } else {
      initializer = Some(expressionStatement())
    }

    var condition : Option[Expr] = None
    // if the next token is not a semi colon
    // then there is a condition
    if(!check(SEMICOLON)){
      condition = Some(expression())
    }
    consume(SEMICOLON, "Expect a ';' after a for loop condition")
    // we expect a statement or a right parenthesis ')' afterwards

    var updateStatement : Option[Expr] = None
    if(!check(RIGHT_PAREN)){
      updateStatement = Some(expression())
    }

    consume(RIGHT_PAREN,"Expect a ')' after for loop")
    var body =  statement()
    if (updateStatement.isDefined){
      // if there is an update statement, run it after running the body
      body = Stmt.Block(List(body,Stmt.Expression(updateStatement.get)))
    }

    // if there is no condition, assume true
    val cond = condition.getOrElse(Expr.Literal(true))

    body = Stmt.While(cond, body)

    // if the initializer is defined, then create a block, running
    // the initializer first
    if(initializer.isDefined){
      body = Stmt.Block(List(initializer.get, body))
    }

    body

  }
  private def whileStatement(): Stmt = {
    /**
     * whileStmt      → "while" "(" expression ")" statement ;
     * */
    consume(LEFT_PAREN,"Expect '(' after 'while'")
    val condition = expression()
    consume(RIGHT_PAREN, "Expect '(' after 'while'")

    val whileStatement = statement()
   // consume(SEMICOLON, "expect a ';' after a while statement")
    Stmt.While(condition,whileStatement)

  }
  private def ifStatement() : Stmt = {
    consume(LEFT_PAREN,"Expect '(' after 'if'")
    val condition = expression()
    consume(RIGHT_PAREN, "Expect ')' after if condition")

    val thenBranch = statement()
    // statement recursively calls inner ifs (if they exist)

    var elseBranch : Option[Stmt] = None
    if(matchAndConsumeType(ELSE)){
      elseBranch = Some(statement())
    }

    Stmt.If(condition,thenBranch,elseBranch)

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
    /**
     * expression     → assignment ;
      assignment     → IDENTIFIER "=" assignment  // where identifier
                     | logic_or ;
      logic_or       → logic_and ( "or" logic_and )* ;
      logic_and      → equality ( "and" equality )* ;
     * */
    assignment()
  }

  private def assignment() : Expr = {

    /**
     * assignment     → ( call "." )? IDENTIFIER "=" assignment
               | logic_or ;
     * */

    // gotta be able to distinguish between
    // var a = 4  CORRECT
    // a + b = c  Not correct
    val leftHand = or()

    if (matchAndConsumeType(EQUAL)){
      val equals = previous()
      val right = assignment()
      leftHand match {
        case e : Expr.Variable =>
          Expr.Assign(e.name, right)

          // for the case where we expect the left side to be
        // breakfast.server.john
        // so the whole of the left hand side should be a GET

        // breakfast.server = "john"

        // callee is breakfast, name is server and value is "john"
        case get : Expr.Get =>
          Expr.Set(get.callee, get.name,right )


        case _ =>
          error(Some(equals), "Invalid assignment target.")
          leftHand
      }
    } else {
      leftHand
    }
  }


  private def or() : Expr = {
    var expr = and()
    while(matchAndConsumeType(OR)){
      val operator = previous()
      val right = and()

      expr = Expr.Logic(expr,operator,right)
    }
    expr
  }

  private def and() : Expr = {
    var expr = equality()
    while(matchAndConsumeType(AND)){
      val operator = previous()
      val right = equality()
      expr = Expr.Logic(expr,operator,right)
    }
    expr
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
    /**
     * unary          → ( "!" | "-" ) unary | call ;
       call           → primary ( "(" arguments? ")" )* ;
        arguments      → expression ( "," expression )* ;
     * */
    if(matchAndConsumeType(BANG,MINUS)){
      val operator = previous()
      val right = unary()
      Expr.Unary(operator,right)
    }else {
      call()
    }
  }

  private def call() : Expr = {
    /**
     * call           → primary ( "(" arguments? ")" | "." IDENTIFIER )* ;
     * */
    var expr = primary()

    var stop = false

    while(!stop){
      if(matchAndConsumeType(LEFT_PAREN)){
        expr = finishCall(expr)
      } else if (matchAndConsumeType(DOT)) {
        val name = consume(IDENTIFIER, "expect property name after '.'.")
        expr = Expr.Get(expr, name)
      }else {
        // break loop
        stop = true
      }
    }
    expr
  }

  private def finishCall(callee: Expr) : Expr = {
    val arguments : ListBuffer[Expr] = ListBuffer.empty
    // could be an empty argument list

    // wanna capture (a , b, c)
//    if(matchAndConsumeType(FUN)){
//     val func =  anonFunc()
//
//    }
     if(!check(RIGHT_PAREN)){
      //
      do {
        if (arguments.length >= 255) {
          error(peek(), "Can't have more than 255 arguments.");
        }
       arguments.append(expression())
      } while (matchAndConsumeType(COMMA))
    }

    val paren = consume(RIGHT_PAREN, "Expect a ')' after arguments ")
    Expr.Call(callee,paren,arguments.toList)
  }

  private def primary() : Expr = {

    /**
     * primary        → "true" | "false" | "nil" | "this"
               | NUMBER | STRING | IDENTIFIER | "(" expression ")"
               | "super" "." IDENTIFIER ;
     * */
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
    } else if (matchAndConsumeType(THIS)){
      Expr.This(previous())
    } else if (matchAndConsumeType(SUPER)){
      val keyword = previous()
      consume(DOT,"Expect a '.' after super")
      val method = consume(IDENTIFIER, "Expect an super class method name after '.'")
      Expr.Super(keyword,method)
    }else throw error(peek(),"Expect Expression")

  }

  private def consume(expected: TokenType, msg: String) : Token={
    if(peek().map(_.tokenType).contains(expected)){
      advance()
    } else throw error(peek(),msg)

  }

  private def error(token : Option[Token], message : String) = {
    println(s"$token: $message")
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
    peek().exists(_.tokenType == EOF)
  }

  private def peek(): Option[Token] = {
    Some(tokens(current))
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
              loop(true)
            case _ => {
              advance()
              loop(isAtEnd)
            }
          }
        }
      }
    }

    loop(isAtEnd())
  }
}
