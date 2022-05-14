package com.craftinginterpreters.lox


import com.craftinginterpreters.lox.TokenType._

import scala.collection.mutable.ListBuffer

case class Scanner(source: String) {
  private val tokens = ListBuffer.empty[Token]

  //val lox = Lox


  private val keywords: Map[String, TokenType] = List(
    ("and", AND),
    ("class", CLASS),
    ("else", ELSE),
    ("false", FALSE),
    ("for", FOR),
    ("fun", FUN),
    ("if", IF),
    ("nil", NIL),
    ("or", OR),
    ("print", PRINT),
    ("return", RETURN),
    ("super", SUPER),
    ("this", THIS),
    ("true", TRUE),
    ("var", VAR),
    ("while", WHILE),
  ).toMap


  private var start = 0
  private var current = 0
  private var line = 1

  private def isAtEnd(): Boolean = {
    current >= source.length
  }

  def scanTokens: List[Token] = {
    while (!isAtEnd()) {
      // after each scan,
      // set current to start
      start = current
      scanToken()

    }
    tokens.append(Token(EOF, "", null, line))
    tokens.toList
  }

  private def scanToken() = {
    // give me char at current + 1
    // while also changing current to current + 1
    val c = advance()
    c match {
      case '(' => addToken(LEFT_PAREN)
      case ')' => addToken(RIGHT_PAREN)

      case '{' => addToken(LEFT_BRACE)

      case '}' => addToken(RIGHT_BRACE)

      case ',' => addToken(COMMA)

      case '.' => addToken(DOT)

      case '-' => addToken(MINUS)

      case '+' => addToken(PLUS)

      case ';' => addToken(SEMICOLON)

      case '*' => addToken(STAR)

      case '!' =>
        // if next char complements this, then use both else this
        // advance current if it complements
        addToken({
          if (matchNextChar('=')) BANG_EQUAL else BANG
        })

      case '=' =>
        addToken({
          if (matchNextChar('=')) EQUAL_EQUAL else EQUAL
        })

      case '<' =>
        addToken({
          if (matchNextChar('=')) LESS_EQUAL else LESS
        })

      case '>' =>
        addToken({
          if (matchNextChar('=')) GREATER_EQUAL else GREATER
        })

      case '/' => {
        // here , it could be divide by or a comment
        if (matchNextChar('/')) {
          // we're in a comment line
          // ignore up to new line '\n'
          while (peek() != '\n' & !isAtEnd()) {
            advance()
          }
        } else {
          addToken(SLASH)
        }
      }

      case ' ' | '\r' | '\t' => // ignore
      case '\n' => {
        //println(s"new line encountered with current is ${source.charAt(current - 2)}: ${source.charAt(current - 1)}")
        line += 1
      }
      case '"' => captureString()

      case _ if c.isDigit =>
        captureNumber()

      case _ if isAlphaOrUnderScore(c) =>
        captureIdentifier()

      case _ =>
        Lox.error(line, s"Unexpected character $c")
    }
  }

  private def captureIdentifier(): Unit = {
    // take all that's a valid identifier probably
    while (isAlphaNumeric(peek())) advance()

    val keyword = source.substring(start, current)

    val tokenType = keywords.getOrElse(keyword, IDENTIFIER) //.getOrElse(IDENTIFIER)
    addToken(tokenType)

  }

  private def isAlphaOrUnderScore(char: Char): Boolean = {
    char.isLetter || char == '_'
  }

  private def isAlphaNumeric(char: Char): Boolean = {
    isAlphaOrUnderScore(char) || char.isDigit
  }

  private def captureString(): Unit = {
    while (peek() != '"' && !isAtEnd()) {
      // if the string is not closing and not at the end
      if (peek() == '\n') {
        line += 1 // support multi line string
      }

      advance()
    }

    if (isAtEnd()) {
      Lox.error(line, "unterminated string")
      return
    }

    // the closing "
    advance()

    // trim surrounding quotes

    val value = source.substring(start + 1, current - 1)
    addToken(STRING, value)
  }
  //
  //  private def captureNumber() : Unit = {
  //    while((peek().isDigit || peek() == '.') && !isAtEnd()){
  //      if (peek() == '.'){
  //        if(!peakNext().isDigit) {
  //          Lox.error(line, s"Invalid number ${source.substring(start, current + 1)}")
  //          advance()
  //          println(s"current is $current")
  //          return
  //        }
  //      }
  //      advance()
  //
  //    }
  //    val value = source.substring(start, current)
  //   // println(s"value is ${value.toDouble}")
  //    addToken(NUMBER,value.toDouble)
  //  }

  private def captureNumber(): Unit = {
    var char = peek()
    while ((char.isDigit || char == '.') && !isAtEnd()) {
      if (char == '.') {
        if (!peakNext().isDigit) {
          Lox.error(line, s"Invalid number ${source.substring(start, current + 1)}")
          advance()
          return
        }
      }
      advance()
      char = peek()

    }
    val value = source.substring(start, current)
    // println(s"value is ${value.toDouble}")
    addToken(NUMBER, value.toDouble)
  }

  private def peakNext() = {
    if (current + 1 >= source.length) {
      '\u0000'
    } else {
      source.charAt(current + 1)
    }
  }

  // this checks if the next character is what's expected and only advances if
  // that's what's expected

  /// like a conditional advance
  private def matchNextChar(expected: Char): Boolean = {
    if (!isAtEnd()) {
      if (source.charAt(current) != expected) {
        false
      } else {
        current += 1
        true
      }
    } else {
      false
    }
  }

  private def advance() = {
    val char = source.charAt(current)
    current += 1
    char
  }

  private def addToken(tokenType: TokenType): Unit = {
    addToken(tokenType, null)
  }

  private def peek(): Char = {
    if (isAtEnd()) {
      '\u0000'
    } else {
      source.charAt(current)
    }
  }

  private def addToken(tokenType: TokenType, literal: Any): Unit = {
    val text = source.substring(start, current)
    tokens.append(Token(tokenType, text, literal, line))
  }
}
