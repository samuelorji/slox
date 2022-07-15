package com.craftinginterpreters.lox



import java.io.{BufferedReader, File, InputStreamReader}
import scala.io.Source

object Lox {
  var hadError = false
  var hadRuntimeError = false

  def runner(args: Array[String]): Unit = {
    args.length match {
      case 1 =>
        runFile(args.head)
      case x if x > 1 =>
        println("Usage: slox [script]")
        sys.exit(64)

      case _ =>
        runPrompt()
        sys.exit(64)
    }
  }

  // lexemes are blobs of characters grouped together into smaller
  // sequences that still represent something


  def runFile(scriptPath: String) = {
    val source = Source.fromFile(new File(scriptPath))
    val sourceCodeString = source.getLines().mkString("\n")
    source.close()
    run(sourceCodeString)
    if (hadError) System.exit(65)
    if(hadRuntimeError) System.exit(70)


  }

  // for interactive mode
  def runPrompt() = {
    val input = new InputStreamReader(System.in)
    val reader = new BufferedReader(input)
    var break = false;
    while (!break) {
      println("> ")
      val line = reader.readLine()
      if (line == null) {
        break = true
      } else {
        run(line)
        hadError = false
      }
    }
  }

  def error(line: Int, message: String): Unit = {
    report(line, "", message)
  }

  def error(tokenOpt : Option[Token], message : String) = {
    tokenOpt match {
      case Some(token) =>
        token.tokenType match {
          case TokenType.EOF =>
            report(token.line, " at end", message)
          case  _ =>
            report(token.line, s"at ${token.lexeme} ",message)
        }
      case None =>
        val line = tokenOpt.map(_.line).getOrElse(-1)
        val lexme = "unknown"
        report(line, " at '" + lexme + "'", message)

    }
  }

  private def report(line: Int, where: String, message: String): Unit = {
    System.err.println("[line " + line + "] Error: " + where + ": " + message)
    hadError = true
  }

  def runtimeError(error : RuntimeError) = {
    println(error.getMessage() +"\n[line " + error.token.line + "]")
    hadRuntimeError = true
  }

  def runIfNoError[A](thunk : => A) :Any = {
    if(!hadError){
      thunk
    } else {
      sys.exit(10)
    }
  }
  def run(source: String) = {
    val scanner = Scanner(source)
    val tokens = scanner.scanTokens

  // println(s"tokens are \n${tokens.mkString("\n")}")
    val parser = Parser(tokens)
    // parses statements
    val statements: Array[Stmt] = parser.parse()

  //  println(Console.GREEN + statements.mkString("\n") + Console.RESET)

    runIfNoError {
      Resolver.resolve(statements.toList)
    }



    runIfNoError {
      MatchInterpreter.interpret(statements)
    }


    // Interpreter.interpret(statements)
   // println(result.accept(Interpreter))
    //tokens.foreach(println)
  }


}
