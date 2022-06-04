package com.craftinginterpreters.lox

trait LoxCallable {
  def arity : Int
  def call (arguments : List[Any])
}
