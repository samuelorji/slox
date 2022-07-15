package com.craftinginterpreters.lox

case class LoxFunction(declaration : Stmt.Function,closure : Environment, isInitializer : Boolean= false) extends LoxCallable {

  override def arity: Int = declaration.params.length

  override def call(interpreter: MatchInterpreter.type, arguments: List[Any]): Any = {
    //println(s"environment is ${closure.environment}")
    val environment = Environment(Some(closure))
    for ((params, args) <- declaration.params zip arguments){
      environment.define(params.lexeme, args)
    }

  //  interpreter.executeBlock(declaration.body, environment)

    try {
      interpreter.executeBlock(declaration.body, environment)
      if(isInitializer) {
        closure.getAt(0,"this")
      }
    } catch {
      case returnedValue: Return =>
        if(isInitializer) {
          closure.getAt(0,"this")
        } else returnedValue.value.orNull
    }


  }

  def bind(instance: LoxInstance) = {
    // bind method binds an instance to this
    val env = Environment(Some(closure))
    env.define("this", instance)
    LoxFunction(declaration,env, isInitializer)
  }

 // override def toString: String = "<fn " + declaration.name.lexeme + ">"
  override def toString: String = "<fn " + declaration.name.lexeme + " "  + closure.environment + ">"

}
