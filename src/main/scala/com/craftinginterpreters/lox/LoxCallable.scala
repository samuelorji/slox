package com.craftinginterpreters.lox

trait LoxCallable {
  def arity : Int
  def call (interpreter: MatchInterpreter.type , arguments : List[Any]) : Any
}
