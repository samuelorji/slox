package com.craftinginterpreters.lox

case class LoxFunction(declaration : Stmt.Function,closure : Environment) extends LoxCallable {
  override def arity: Int = declaration.params.length

  override def call(interpreter: MatchInterpreter.type, arguments: List[Any]): Any = {
    val environment = Environment(Some(closure))
    for ((params, args) <- declaration.params zip arguments){
      environment.define(params.lexeme, args)
      println(environment)
    }

    interpreter.executeBlock(declaration.body,environment)
  }

  override def toString: String = "<fn " + declaration.name.lexeme + ">"
}
