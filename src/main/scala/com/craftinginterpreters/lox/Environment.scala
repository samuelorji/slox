package com.craftinginterpreters.lox

case class Environment(val enclosing : Option[Environment] = None ){
   var environment = Map.empty[String,Any]

  def define(identifier: String, value : Any) = {
    environment = environment.updated(identifier,value)
  }
  
  def getAt(distance : Int, token : Token) = {
    ancestor(distance).flatMap(_.environment
      .get(token.lexeme)).getOrElse{
      throw RuntimeError(token,"Undefined variable '" + token.lexeme + "'.")
    }
  }

  def getAt(distance : Int, token : String) = {
    ancestor(distance).flatMap(_.environment
      .get(token)).get
  }
  
  def ancestor(distance : Int) : Option[Environment] = {
    var env: Option[Environment] = Some(this) 
    var i = 0 
    while(i < distance) {
      env = env.flatMap(_.enclosing)
      i += 1
    }
    env
  }

  def assignAt(distance : Int, name : Token , value : Any) = {
    ancestor(distance).map(_.define(name.lexeme,value))
  }

  def get(token : Token) : Any = {
    environment.getOrElse(token.lexeme, {
      enclosing.map(_.get(token)).getOrElse {
        throw RuntimeError(token, "Undefined variable '" + token.lexeme + "'.")
      }
    })
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

 // override def toString: String = s"$environment"
}
