package com.craftinginterpreters.lox

case class LoxFunction(declaration : Stmt.Function,closure : Environment) extends LoxCallable {
  override def arity: Int = declaration.params.length

  override def call(interpreter: MatchInterpreter.type, arguments: List[Any]): Any = {
    //println(s"environment is ${closure.environment}")
    val environment = Environment(Some(closure))
    for ((params, args) <- declaration.params zip arguments){
      environment.define(params.lexeme, args)
      println(environment)
    }

    interpreter.executeBlock(declaration.body,environment)
  }

  def bind(instance: LoxInstance) = {
    val env = Environment(Some(closure))
    env.define("this", instance)
    LoxFunction(declaration,env)
  }

 // override def toString: String = "<fn " + declaration.name.lexeme + ">"
  override def toString: String = "<fn " + declaration.name.lexeme + " "  + closure.environment + ">"

}
