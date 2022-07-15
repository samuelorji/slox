package com.craftinginterpreters.lox

case class LoxClass(name : String, methods : scala.collection.mutable.Map[String,LoxFunction]) extends LoxCallable {


  override def arity: Int = 0

  override def call(interpreter: MatchInterpreter.type, arguments: List[Any]): Any = {
    LoxInstance(this)
  }

  def findMethod(methodName : String) =
    methods.get(methodName)

  override def toString: String = {
    name
  }
}
