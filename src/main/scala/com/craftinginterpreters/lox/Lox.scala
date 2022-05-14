package com.craftinginterpreters.lox


import java.io.{BufferedReader, File, InputStreamReader}
import scala.io.Source

object Lox {
  var hadError = false

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

  private def report(line: Int, where: String, message: String): Unit = {
    System.err.println("[line " + line + "] Error" + where + ": " + message)
    hadError = true
  }

  def run(source: String) = {
    val scanner = Scanner(source)
    val tokens = scanner.scanTokens
    //tokens.foreach(println)
  }


}
