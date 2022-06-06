package com.craftinginterpreters.lox

case class Return(value : Option[Any]) extends RuntimeException(null,null,false,false)
