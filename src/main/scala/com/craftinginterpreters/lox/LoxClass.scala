package com.craftinginterpreters.lox

case class LoxClass(name : String, methods : scala.collection.mutable.Map[String,LoxFunction]) extends LoxCallable {


  override def arity: Int =  {
    findMethod("init").map(_.arity).getOrElse(0)
  }

  override def call(interpreter: MatchInterpreter.type, arguments: List[Any]): Any = {
    val instance = LoxInstance(this)
    val initializerOpt = findMethod("init")
    initializerOpt.map {initializer =>
      initializer.bind(instance).call(interpreter,arguments)
    }

    instance
  }

  def findMethod(methodName : String) =
    methods.get(methodName)

  override def toString: String = {
    name
  }
}
