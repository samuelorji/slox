package com.craftinginterpreters.lox
//
//import scala.reflect.ClassTag
//
case class RuntimeError(token: Token, message: String) extends RuntimeException(message,null,false,false)
//
//object Interpreter extends Expr.Visitor[Any]
//  with Stmt.Visitor[Unit]  // statements don't return anything
//  {
//
//
//    override def visitCallExpr(expr: Expr.Call): Any = ???
//
//    override def visitWhileStmt(stmt: Stmt.While): Unit = ???
//
//    override def visitLogicExpr(expr: Expr.Logic): Any = ???
//
//    var environment = Environment()
//
//
//    override def visitIfStmt(stmt: Stmt.If): Unit = ???
//
//    override def visitExpressionStmt(stmt: Stmt.Expression): Unit = {
//    println(s"got an expression statement ${stmt.expression}")
//    val expressionResult = evaluate(stmt.expression)
//    println(s"result is $expressionResult")
//
//  }
//
//
//  override def visitBlockStmt(stmt: Stmt.Block): Unit = {
//    executeBlock(stmt.statements,Environment(Some(environment)))
//  }
//
//    // each block gets its own environment
//  private def executeBlock(statements : List[Stmt], environment: Environment): Unit = {
//    // save current environment as blocks introduce their environments
//    val previous = this.environment
//    try {
//      // this block is introducing its own environment, so we set the current environment
//      // to the one for the block
//
//      // if there are multiple blocks, each will introduce its own environment
//      this.environment = environment
//      statements.foreach(execute)
//    } finally {
//      // we're done executing blocks, now set our current environment to the outer environment
//      this.environment = previous
//    }
//  }
//
//  override def visitAssignExpr(expr: Expr.Assign): Any = {
//    val exprResult = evaluate(expr.value)
//    environment.assign(expr.name,exprResult)
//    exprResult
//  }
//
//
//  override def visitPrintStmt(stmt: Stmt.Print): Unit = {
//    val expressionResult = evaluate(stmt.expression)
//    println(stringify(expressionResult))
//  }
//
//  override def visitBinaryExpr(expr: Expr.Binary): Any = {
//    val left = evaluate(expr.left)
//    val right = evaluate(expr.right)
//    val operator : Token = expr.operator
//
//     operator.tokenType match {
//      case TokenType.PLUS =>
//
//        left match {
//          case _ : String if right.isInstanceOf[String] =>
//            left +  right.asInstanceOf[String]
//          case _ : String if right.isInstanceOf[Double] =>
//            left.asInstanceOf[String] + right.asInstanceOf[Double]
//          case _: Double if right.isInstanceOf[Double] =>
//            left.asInstanceOf[Double] + right.asInstanceOf[Double]
//          case _ =>
//            throw RuntimeError(operator, "Operands must be two numbers or two strings.")
//        }
//      case TokenType.MINUS =>
//        checkOperands(operator,left,right)
//        left.asInstanceOf[Double] - right.asInstanceOf[Double]
//      case TokenType.STAR =>
//        checkOperands(operator,left,right)
//        left.asInstanceOf[Double] * right.asInstanceOf[Double]
//      case TokenType.SLASH =>
//        checkOperands(operator,left,right)
//        val rightNumber = right.asInstanceOf[Double]
//        if (rightNumber == 0){
//          throw RuntimeError(operator, "Cannot Divide by 0")
//        }
//        left.asInstanceOf[Double] / right.asInstanceOf[Double]
//      case TokenType.GREATER =>
//        checkOperands(operator,left,right)
//        left.asInstanceOf[Double] > right.asInstanceOf[Double]
//      case TokenType.GREATER_EQUAL =>
//        checkOperands(operator,left,right)
//        left.asInstanceOf[Double] >= right.asInstanceOf[Double]
//      case TokenType.LESS =>
//        checkOperands(operator,left,right)
//        left.asInstanceOf[Double] < right.asInstanceOf[Double]
//      case TokenType.LESS_EQUAL =>
//        checkOperands(operator,left,right)
//        left.asInstanceOf[Double] <= right.asInstanceOf[Double]
//      case TokenType.BANG_EQUAL =>
//        !isEqual(left,right)
//      case TokenType.EQUAL_EQUAL =>
//         isEqual(left,right)
//    }
//
//  }
//
//  override def visitGroupingExpr(expr: Expr.Grouping): Any = {
//    evaluate(expr.expression)
//  }
//
//  override def visitLiteralExpr(expr: Expr.Literal): Any = {
//    expr.value
//  }
//
//  override def visitUnaryExpr(expr: Expr.Unary): Any = {
//    val right = evaluate(expr.right)
//    expr.operator.tokenType match {
//      case TokenType.MINUS =>
//        checkOperands[Double](expr.operator,right)
//        - right.asInstanceOf[Double]
//      case TokenType.BANG =>
//        !isTruthy(right)
//    }
//  }
//
//  private def checkOperands[A](token : Token, operands : Any*)(implicit tag : ClassTag[A]) : Unit = {
//    if(operands.forall(_.isInstanceOf[A])){
//    } else {
//      throw new RuntimeError(token, s"Operand must be of type ${tag.runtimeClass.getName}")
//    }
//  }
//  private def isEqual(a : Any, b: Any) : Boolean = {
//    if (a == null && b == null){
//      true
//    } else if (a == null) {
//      false
//    } else {
//      a equals b
//    }
//  }
//
//  private def isTruthy(value: Any): Boolean = {
//    if (value == null) {
//      false
//    }else value match {
//      case bool: Boolean =>
//        bool
//      case _ =>
//        true
//    }
//  }
//  private def evaluate(expression : Expr) : Any = {
//    expression.accept(this)
//
//  }
//
//  def interpret(expression : Expr) = {
//    try {
//      val result = evaluate(expression)
//      println(stringify(result))
//    } catch {
//      case e : RuntimeError =>
//        Lox.runtimeError(e)
//    }
//  }
//
//  def interpret(statements : Array[Stmt]) = {
//    try {
//      statements.foreach(execute)
//    }catch {
//      case  e : RuntimeError =>
//        Lox.runtimeError(e)
//    }
//  }
//
//  private def execute(stmt: Stmt) = {
//    stmt.accept(this)
//  }
//
//  private def stringify(result : Any) : String = {
//    if (result == null) {
//      "nil"
//    } else {
//      result match {
//        case x : Double =>
//          val result = x.toString
//
//          if (result.endsWith(".0")){
//            result.take(result.length -2)
//          } else {
//            result
//          }
//        case _ =>
//          result.toString
//      }
//    }
//  }
//
//  override def visitVariableExpr(expr: Expr.Variable): Any = {
//    // for a variable, this will probably be invoked or called
//    // so it needs to be evaluated and returned to its caller
//
//    // example, if it were a print,
//    // visiting a print statment will evaluate the expression to be printed
//     // which will eventually come here and the result is printed by print
//
//    // to show an example, try returning anything else and see it printed
//    environment.get(expr.name)
//  }
//
//  override def visitVarStmt(stmt: Stmt.Var): Unit = {
//    var expressionValue : Any = null
//    if(stmt.initializer != null){
//      expressionValue = evaluate(stmt.initializer)
//    }
//    environment.define(stmt.name.lexeme,expressionValue)
//  }
//}
