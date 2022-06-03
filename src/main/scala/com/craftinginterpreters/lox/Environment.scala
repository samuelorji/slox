package com.craftinginterpreters.lox

case class Environment(enclosing : Option[Environment] = None ){
  private var environment = Map.empty[String,Any]

  def define(identifier: String, value : Any) = {
    environment = environment.updated(identifier,value)
  }

  def get(token : Token) : Any = {
    environment.getOrElse(token.lexeme, enclosing.map(_.get(token)).getOrElse(throw RuntimeError(token,"Undefined variable '" + token.lexeme + "'.")))
  }

  def assign(token : Token , value : Any) : Unit = {
    // environment here is the environment of the current scope
    if(environment.contains(token.lexeme)){
      environment = environment.updated(token.lexeme, value)
    } else if (enclosing.nonEmpty){
      enclosing.foreach(_.assign(token,value))
    } else {
      throw RuntimeError(token,
        "Undefined variable '" + token.lexeme + "'.");
    }
  }
}
