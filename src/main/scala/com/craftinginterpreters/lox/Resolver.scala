package com.craftinginterpreters.lox
import scala.collection.mutable

object Resolver {
  private val scopes = mutable.Stack.empty[mutable.Map[String,Boolean]]
  private var currentFunction: FunctionType = FunctionType.None

  sealed trait FunctionType
  object FunctionType  {
    case object None extends FunctionType
    case object Function extends FunctionType
    case object Method extends FunctionType
    case object Initializer extends FunctionType
  }

  sealed trait ClassType
  object ClassType {
    case object None extends ClassType
    case object CLASS extends ClassType
  }

  private var currentClass : ClassType = ClassType.None

  def evaluateExpression(expr : Expr)  = {

    expr match {

      case p@Expr.Variable(name) =>
        // to prevent
        // var a = a
        if(scopes.nonEmpty && scopes.top.get(name.lexeme).contains(false)){

          Lox.error(Some(name),"Can't read local variable in its own initializer")
        }

       // println(s"resolving variable, scopes : ${scopes}")
        resolveLocal(p, name)

      case p@Expr.Assign(name, value) =>

        resolve(value)
        resolveLocal(p, name)

      case Expr.Binary(left, operator, right) =>
        resolve(left)
        resolve(right)

      case Expr.Call(callee, paren, arguments) =>
        resolve(callee)
        arguments.foreach(resolve)

      case Expr.Grouping(expression) =>
        resolve(expression)
      case Expr.Literal(_) =>

      case Expr.Logic(left, operator, right) =>
        resolve(left)
        resolve(right)

      case Expr.Unary(operator, right) =>
        resolve(right)

      case Expr.Get(callee, name) =>
        resolve(callee)

      case Expr.Set(obj, name, value) =>
        resolve(obj)
        resolve(value)

      case expr @Expr.This(keyword) =>
        currentClass match {
          case ClassType.None =>
            Lox.error(Some(keyword),"Cannot use 'this' outside a class")
          case _ =>
            resolveLocal(expr, keyword)
        }



      case _ => throw new IllegalStateException(s"don't know expr $expr")

    }
  }

  def resolveLocal(variable: Expr, token: Token): Unit = {

    val innerScope = scopes.toArray.reverse

    var i = innerScope.size - 1

    var stop = false
    /**
     * If we find the variable, we resolve it,
     * passing in the number of scopes between the current innermost scope
     * and the scope where the variable was found
     * */
    while(!stop && i >= 0){
      if(innerScope(i).contains(token.lexeme)){
        MatchInterpreter.resolve(variable,innerScope.size - 1 - i)
        stop = true
      }
      i -= 1
    }
  }

  def resolveFunction(func: Stmt.Function, functionType: FunctionType) = {
    val enclosingFunction = currentFunction
    currentFunction = functionType
    beginScope()
    func.params.foreach {param =>
      declare(param)
      define(param)
    }

    resolve(func.body)
    endScope()
    currentFunction = enclosingFunction

  }

  def evaluateStatement(stmt: Stmt) = {
    //println(s"statement is $stmt")
    stmt match {
      case Stmt.Block(statements) =>
        beginScope()
        resolve(statements)
        endScope()
      case Stmt.Var(name, initializer) =>
        declare(name)
        if(initializer != null){
          //println(s"initializer is $initializer")
          resolve(initializer)
        }
        define(name)

      case p@Stmt.Function(name, _, _) =>
        declare(name)
        define(name)
        resolveFunction(p,FunctionType.Function)

      case Stmt.Expression(expression) =>
        resolve(expression)

      case Stmt.If(condition, thenBranch, elseBranch) =>
        resolve(condition)
        resolve(thenBranch)
        elseBranch.foreach(resolve)

      case Stmt.Print(expression) =>
        resolve(expression)
      case Stmt.Return(keyword, value) =>
        currentFunction match {
          case FunctionType.None =>
            Lox.error(Some(keyword),"Can't return from top level code")
          case FunctionType.Initializer =>
            // report lox error if return has a value
            value.foreach(_ => Lox.error(Some(keyword),"Can't return a value from an initializer"))

          case _ =>
        }
        value.foreach(resolve)
      case Stmt.While(condition, statement) =>
        resolve(condition)
        resolve(statement)

      case Stmt.Class(name, methods) =>
        val enclosingClass = currentClass
        currentClass = ClassType.CLASS
        declare(name)
        define(name)
        beginScope()
        scopes.top.put("this",true)
        methods.foreach { method =>
          val functionType = if(method.name.lexeme.contentEquals("init")){
            FunctionType.Initializer
          } else FunctionType.Method
          resolveFunction(method,functionType)
        }
        endScope()

        currentClass = enclosingClass

      case _ =>
        throw new IllegalStateException(s"Unknown stmt $stmt")


    }
  }

  def declare(token: Token) = {
    if(scopes.nonEmpty){
      val currentScope = scopes.top
      //marked as not ready yet
      if(currentScope.contains(token.lexeme)){
        Lox.error(Some(token), "Already a variable with this name in this scope.");
      }else {
        currentScope.put(token.lexeme, false)
      }
    }

  }

  def define(token : Token) = {
    if(scopes.nonEmpty){
      scopes.top.put(token.lexeme,true)
    }
  }
  def beginScope() = {
     scopes.push(mutable.Map.empty)
  }

  def endScope() = {
    scopes.pop()
  }

  def resolve(statements: List[Stmt]) : Any = {
    statements.foreach(resolve)
  }

  def resolve(stmt: Stmt) : Any = {
    evaluateStatement(stmt)

  }

  def resolve(expr : Expr) : Any = {
    evaluateExpression(expr)
  }

}