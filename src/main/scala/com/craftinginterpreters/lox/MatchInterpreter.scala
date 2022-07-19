package com.craftinginterpreters.lox
import TokenType._

import scala.collection.mutable
//import com.craftinginterpreters.lox.Interpreter.checkOperands

import scala.reflect.ClassTag

object MatchInterpreter extends InterpreterHelper {

  val globals = Environment()

  private var environment = globals
  private var locals = Map.empty[Expr,Int]

  globals.define("clock", new LoxCallable {
    override def arity: Int = 0

    override def call(interpreter: MatchInterpreter.type , arguments: List[Any]): Unit =
      System.currentTimeMillis().toDouble / 1000.0

    override def toString: String = {
      "<native fn>"
    }
  })

  def interpret(statements : Array[Stmt]) : Unit = {
    try {
      statements.foreach(execute)
    } catch {
      case e: RuntimeError =>
        Lox.runtimeError(e)
    }
  }


  private def execute(statement : Stmt) : Unit = {
    statement match {
      case Stmt.Block(statements) =>
        executeBlock(statements,Environment(Some(environment)))

      case Stmt.Expression(expression) =>
        // could be a side effecting thing, but the result is hardly used
        evaluateExpression(expression)
      case Stmt.Print(expression) =>
        val expressionResult = evaluateExpression(expression)
        println(stringify(expressionResult))

      case Stmt.Var(name, initializer) =>
        var expressionResult : Any = null
        if(initializer != null) {
          expressionResult = evaluateExpression(initializer)
        }
        environment.define(name.lexeme,expressionResult)

      case Stmt.If(cond, thenBranch, elseBranch) =>
        val condition = evaluateExpression(cond)

        if(isTruthy(condition)){
          execute(thenBranch)
        }else {
          elseBranch.foreach(execute)
        }

      case Stmt.While(condition, statement) =>
        // expression should not be memoized, but passed into the
        // while loop so its constantly evaluated
        while (isTruthy(evaluateExpression(condition))) {
          execute(statement)
        }

      case func: Stmt.Function =>
        // function should take a closure
        val loxFunction = LoxFunction(func,environment)
        environment.define(func.name.lexeme, loxFunction)

      case Stmt.Return(_, value) =>
        val returnValue = value.map(evaluateExpression)
        throw Return(returnValue)

      case Stmt.Class(name, methods,_superClass) =>
        val sup = _superClass.map{supClass =>
          val superClass = evaluateExpression(supClass)

          superClass match {
            case klass : LoxClass => klass
            case _ =>
              //Lox.error("Cannot")
            throw RuntimeError(supClass.name,s"${supClass.name.lexeme} is not a class, Can only extend classes")
          }

        }
        environment.define(name.lexeme,null)

        _superClass.foreach{ _ =>
          // define a new env where we store the super class, so the methods inherit it
          environment = Environment(Some(environment))
          environment.define("super",sup.get)

        }
        val methodsMap = mutable.Map.empty[String,LoxFunction]
        methods.foreach {method =>
          val loxFunction = LoxFunction(method,environment,method.name.lexeme.contentEquals("init"))
          methodsMap.put(method.name.lexeme, loxFunction)
        }

        _superClass.foreach{ _ =>
          environment = environment.enclosing.get
        }
        val klass = LoxClass(name.lexeme, methodsMap,sup)
        environment.assign(name,klass)

      case _ => throw new IllegalStateException(s"Unexpected statement type : $statement")
    }

  }

  private def evaluateExpression(expression: Expr) : Any = {
    expression match {
      /**
       * var a = 5;
       * var b = (4 + 5);
       * */
      case Expr.Assign(token, expr) =>
        // evaluate expr
        val exprResult = evaluateExpression(expr)
        environment.assign(token,exprResult)
      case Expr.Binary(leftExpr, operator, rightExpr) =>
        /**
         * 3 - 4
         * big - small
         * "hello" + "world"
         *
         * */

        // here we check the operator as well as the operands/ result
        // to determine if its the right operation
        val leftResult = evaluateExpression(leftExpr)
        val rightResult = evaluateExpression(rightExpr)

        operator.tokenType match {
          case TokenType.PLUS =>
            leftResult match {
              case _: String if rightResult.isInstanceOf[String] =>
                // treat both as Strings
                leftResult + rightResult.asInstanceOf[String]
              case _: String if rightResult.isInstanceOf[Double] =>
                // treat as a String Concat with double
                leftResult.asInstanceOf[String] + rightResult.asInstanceOf[Double]
              case _: Double if rightResult.isInstanceOf[Double] =>
                // treat as two doubles
                leftResult.asInstanceOf[Double] + rightResult.asInstanceOf[Double]
              case _ =>

                throw RuntimeError(operator, "Operands must be two numbers or two strings.")
            }

          case TokenType.MINUS =>
            checkOperands[Double](operator, leftResult, rightResult)
            leftResult.asInstanceOf[Double] - rightResult.asInstanceOf[Double]
          case TokenType.STAR =>
            leftResult match {
              case left : String if rightResult.isInstanceOf[Double] =>
                try {
                  val right = rightResult.asInstanceOf[Int]
                  left * right
                } catch {
                  case _ => throw RuntimeError(operator, "can only be applied to Whole numbers ")
                }

              case  _ =>
                checkOperands[Double](operator, leftResult, rightResult)
                if (rightResult == 0){
                  throw RuntimeError(operator, "Cannot Divide by 0")
                }
                leftResult.asInstanceOf[Double] * rightResult.asInstanceOf[Double]
            }


          case TokenType.SLASH =>
            checkOperands[Double](operator, leftResult, rightResult)
            leftResult.asInstanceOf[Double] / rightResult.asInstanceOf[Double]

          case TokenType.GREATER =>
            checkOperands[Double](operator,leftResult,rightResult)
            leftResult.asInstanceOf[Double] > rightResult.asInstanceOf[Double]
          case TokenType.GREATER_EQUAL =>
            checkOperands[Double](operator,leftResult,rightResult)
            leftResult.asInstanceOf[Double] >= rightResult.asInstanceOf[Double]
          case TokenType.LESS =>
            checkOperands[Double](operator,leftResult,rightResult)
            leftResult.asInstanceOf[Double] < rightResult.asInstanceOf[Double]
          case TokenType.LESS_EQUAL =>
            checkOperands[Double](operator,leftResult,rightResult)
            leftResult.asInstanceOf[Double] <= rightResult.asInstanceOf[Double]

          case TokenType.BANG_EQUAL =>
            !isEqual(leftResult,rightResult)
          case TokenType.EQUAL_EQUAL =>
            isEqual(leftResult,rightResult)


        }
      case Expr.Grouping(expression) =>
        // evaluate the expression in the grouping
        evaluateExpression(expression)

        /**
         * 1;
         * "Hello"
         * */
      case Expr.Literal(value) =>
        // just return the value
        value

        /**
         * - 1;
         * ! variable
         * operator can only be - or !
         * */
      case Expr.Unary(operator, expression) =>
        // the right is evaluated, because it could be a literal
        // or an expression as in
        // -1 or -age where age  is a variable
        // so by evaluating the expression, we get the result of the expression
        // which if the operator is '-', then we expect that result to be a double

        val expressionResult = evaluateExpression(expression)
        operator.tokenType match {
          case MINUS =>
            // expression result must be of type Double
          checkOperands[Double](operator,expressionResult)
          - expressionResult.asInstanceOf[Double]
          case BANG =>
            !isTruthy(expressionResult)
        }

      case p@Expr.Variable(name) =>
        // get this variable from the environment
        lookupVariable(name,p)
      //environment.get(name)
      case Expr.Logic(left, operator, right) =>
        val leftResult = evaluateExpression(left)
       val result =  operator.tokenType match {
          case OR =>
            if(isTruthy(leftResult)){
              // or case, if left is true , return it
              Left(leftResult)
            } else {
              Right()
            }
          case AND =>
            if(!isTruthy(leftResult)){
              // and case, if left is false, no need to evaluate right
              Left(leftResult)
            } else {
              Right()
            }
          case x =>
            throw new IllegalStateException(s"Unexpected operator type  : $x")
        }

        // only evaluate right if we don't have to return left
        result.fold(identity, _ => evaluateExpression(right))

      case Expr.Call(_callee, paren, arguments) =>

        val callee = evaluateExpression(_callee)

        callee match {
          case callable : LoxCallable =>
            if(arguments.length != callable.arity){
              throw RuntimeError(paren, s"Expected ${callable.arity} arguments but got ${arguments.length} instead")
            } else {
              val args = arguments.map(evaluateExpression)
              callable.call(MatchInterpreter,args)
//              try {
//                callable.call(MatchInterpreter,args)
//              } catch {
//                case ex : Return =>
//                  ex.value.orNull
//              }
            }

          case ex =>
            println(s"ex is $ex")
            throw RuntimeError(paren, "Can only call functions and classes.");
        }

      case Expr.Get(_callee, name) =>
        val callee = evaluateExpression(_callee)
        callee match {
          case instance : LoxInstance =>
            val method = instance.get(name)
            method

          case _ =>
            throw  RuntimeError(name, "Only instances have properties.");
        }

      case Expr.Set(obj, name, value) =>
        val callee = evaluateExpression(obj)
        callee match {
          case instance : LoxInstance =>
            val setValue = evaluateExpression(value)
            instance.set(name.lexeme,setValue)

          case _ =>
            throw  RuntimeError(name, "Only instances have fields.");
        }

      case expr@ Expr.This(keyword) =>
        lookupVariable(keyword,expr)

      case expr@ Expr.Super(keyword, method) =>
        val distance = locals(expr)
        val superClass = environment.getAt(distance,keyword).asInstanceOf[LoxClass]
        val obj = environment.getAt(distance - 1, "this").asInstanceOf[LoxInstance]

        val superMethod = superClass.findMethod(method.lexeme)
        superMethod match {
          case Some(method) =>
            method.bind(obj)
          case None =>
            throw  RuntimeError(expr.method,
              "Undefined property '" + expr.method.lexeme + "'.");
        }


      case _ =>
        throw new IllegalStateException(s"Unexpected expression type : $expression")

    }
  }

  private def checkOperands[A](token : Token, operands : Any*)(implicit tag : ClassTag[A]) : Unit = {
    if(operands.forall(_.isInstanceOf[A])){
    } else {
      throw  RuntimeError(token, s"Operand must be of type ${tag.runtimeClass.getName}")
    }
  }
  def executeBlock(statements : List[Stmt] , environment: Environment) = {
    // a block has its own environment
    // so before we use that, let's save the outermost environment, so once the block is
    // finished, we restore the previous environment

    val previousEnvironment = this.environment

    try {
      // this block is introducing  and using its own env, so set the env
      // in this scope to be for the block
      this.environment = environment
      statements.foreach(execute)
    } finally  {
      // block is complete, now set env to outer most env
      this.environment = previousEnvironment
    }
  }

  def resolve(variable : Expr, depth : Int) = {

   // println(s"resolving variable $variable at depth $depth")
    locals = locals.updated(variable,depth)
  }

  def lookupVariable(token: Token, variable: Expr) = {
    locals.get(variable) match {
      case Some(distance) =>
        environment.getAt(distance, token)
      case None =>
        globals.get(token)
    }

  }

}

trait InterpreterHelper {
  protected def stringify(result : Any) : String = {
    if (result == null) {
      "nil"
    } else {
      result match {
        case x : Double =>
          val result = x.toString

          if (result.endsWith(".0")){
            result.take(result.length -2)
          } else {
            result
          }
        case _ =>
          result.toString
      }
    }
  }

  protected def isTruthy(value: Any): Boolean = {
    if (value == null) {
      false
    }else value match {
      case bool: Boolean =>
        bool
      case _ =>
        true
    }
  }

  protected def isEqual(a : Any, b: Any) : Boolean = {
    if (a == null && b == null){
      true
    } else if (a == null) {
      false
    } else {
      a equals b
    }
  }

}
