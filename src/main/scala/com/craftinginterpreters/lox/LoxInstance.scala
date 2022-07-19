package com.craftinginterpreters.lox

import scala.collection.mutable

case class LoxInstance(klass : LoxClass,superClass : Option[LoxClass]) {
  private val fields = mutable.Map.empty[String, Any]
  override def toString: String = {
    klass.name + " instance"
  }

  def get(name : Token) : Any = {
    { fields.get(name.lexeme) orElse klass.findMethod(name.lexeme).map{
      method =>
     method.bind(this)
    } orElse {
      superClass.flatMap(_.findMethod(name.lexeme).map{
        method =>
          method.bind(this)
      }
      )
    }
    }.getOrElse( throw RuntimeError(name, "Undefined property '" + name.lexeme + "'."))
  }

  def set(name : String, value : Any) =
    fields.put(name,value)
}
